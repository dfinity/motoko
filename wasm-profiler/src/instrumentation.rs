use parity_wasm::builder;
use parity_wasm::elements::{
    BlockType, FuncBody, GlobalEntry, GlobalType, InitExpr, Instruction, Instructions, Internal,
    Local, Module, Section,  ValueType,
    ImportCountType,
};
use std::collections::HashMap;
use std::error::Error;
use std::iter::FromIterator;

// Converts a Wasm instruction to a string mnemonic.
fn instruction_to_mnemonic(i: &Instruction) -> String {
    let out = i.to_string();
    let mut iter = out.split_whitespace();
    iter.next()
        .expect("The string representation of a Wasm instruction is never empty.")
        .to_string()
}

/// The metering can be configured by providing a cost-per-instruction table and
/// the default cost for an instruction in case it's not present in the cost
/// table.
pub struct InstructionCostTable {
    // mapping of instruction mnemonic to its cost
    instruction_cost: HashMap<String, u64>,
    // default cost of an instruction (if not present in the cost table)
    default_cost: u64,
}

impl InstructionCostTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_default_cost(mut self, cost: u64) -> Self {
        self.default_cost = cost;
        self
    }

    pub fn with_instruction_cost(mut self, id: String, cost: u64) -> Self {
        self.instruction_cost.insert(id, cost);
        self
    }

    // Returns the cost of a Wasm instruction from the cost table or the default
    // cost if the instruction is not in the cost table.
    fn cost(&self, i: &Instruction) -> u64 {
        let mnemonic = instruction_to_mnemonic(i);
        *self
            .instruction_cost
            .get(&mnemonic)
            .unwrap_or(&self.default_cost)
    }
}

impl Default for InstructionCostTable {
    fn default() -> Self {
        let mut instruction_cost = HashMap::new();

        // The following instructions are mostly signaling the start/end of code blocks,
        // so we assign 0 cost to them.
        instruction_cost.insert(
            instruction_to_mnemonic(&Instruction::Block(BlockType::NoResult)),
            0,
        );
        instruction_cost.insert(instruction_to_mnemonic(&Instruction::Else), 0);
        instruction_cost.insert(instruction_to_mnemonic(&Instruction::End), 0);
        instruction_cost.insert(
            instruction_to_mnemonic(&Instruction::Loop(BlockType::NoResult)),
            0,
        );

        Self {
            default_cost: 1,
            instruction_cost,
        }
    }
}

fn inject_imports(for_ic : bool, module: Module) -> Module {
    let names_section = module.names_section().expect("names section").clone();
    let mut builder = builder::from_module(module);
    builder = builder.with_section(Section::Name(names_section));

    let mut fn_shift = 0;

    if for_ic {
        let import_sig = builder.push_signature(
            builder::signature()
                .with_param(ValueType::I32)
                .with_param(ValueType::I32)
                .build_sig(),
        );
        builder.push_import(
            builder::import()
                .module("ic0")
                .field("debug_print")
                .external()
                .func(import_sig)
                .build(),
        );
        fn_shift += 1;
    } else {
        let import_sig = builder.push_signature(
            builder::signature()
                .with_param(ValueType::I32)
                .with_param(ValueType::I32)
                .with_param(ValueType::I32)
                .with_param(ValueType::I32)
                .with_result(ValueType::I32)
                .build_sig(),
        );
        builder.push_import(
            builder::import()
                .module("wasi_unstable")
                .field("fd_write")
                .external()
                .func(import_sig)
                .build(),
        );
        fn_shift += 1;
    }

    let mut module = builder.build();
    assert!(module.has_names_section());

    // Lets move the new imports from the end to the beginning of the import list
    let entries = module.import_section_mut().unwrap().entries_mut();
    for _ in 0..fn_shift {
        let last = entries.pop().unwrap();
        entries.insert(0, last);
    }

    // We lift all call references by fn_shift
    for section in module.sections_mut() {
        match section {
            Section::Code(ref mut code_section) => {
                for func_body in code_section.bodies_mut() {
                    let code = func_body.code_mut();
                    code.elements_mut().iter_mut().for_each(|instr| {
                        if let Instruction::Call(ref mut call_index) = instr {
                            *call_index += fn_shift
                        }
                    });
                }
            }
            Section::Export(ref mut export_section) => {
                for export in export_section.entries_mut() {
                    if let Internal::Function(ref mut func_index) = export.internal_mut() {
                        *func_index += fn_shift
                    }
                }
            }
            Section::Element(ref mut elements_section) => {
                for segment in elements_section.entries_mut() {
                    for func_index in segment.members_mut() {
                        *func_index += fn_shift
                    }
                }
            }
            Section::Start(ref mut func_index) => *func_index += fn_shift,
            Section::Name(ref mut name_section) => {
                if let Some(fns) = name_section.functions_mut() {
                    let names_map = fns.names().clone();
                    *fns.names_mut() = parity_wasm::elements::IndexMap::from_iter(
                        names_map.into_iter().map(|(k, v)| (k + fn_shift, v)),
                    )
                }
                if let Some(ls) = name_section.locals_mut() {
                    let names_map = ls.local_names().clone();
                    *ls.local_names_mut() = parity_wasm::elements::IndexMap::from_iter(
                        names_map.into_iter().map(|(k, v)| (k + fn_shift, v)),
                    )
                }
            }
            _ => {}
        }
    }
    module
}

/// Takes a Wasm binary and inserts the instructoing metering and profiling code.
/// Returns  the instrumented binary.
pub fn instrument(
    wasm: &Vec<u8>,
    for_ic: bool,
    instruction_cost_table: &InstructionCostTable,
) -> Result<Vec<u8>, Box<dyn Error>> {
    let module = parity_wasm::deserialize_buffer::<Module>(wasm.as_slice())?;
    let module = module
        .parse_names()
        .map_err(|(mut es, _)| es.pop().unwrap().1)?;
    assert!(module.has_names_section());
    let mut module = inject_imports(for_ic, module);
    assert!(module.has_names_section());
    let num_functions = module.functions_space() as u32;
    let num_globals = module.globals_space() as u32;

    let cycles_counter_ix = num_globals;
    let print_profiling_fn = num_functions;

    // inject cycles counter decrementation
    {
        if let Some(code_section) = module.code_section_mut() {
            for func_body in code_section.bodies_mut().iter_mut() {
                let code = func_body.code_mut();
                inject_metering(code, instruction_cost_table, cycles_counter_ix);
            }
        }
    }

    // inject prinitng of counter
    {
        let n_fun_imports = module.import_count(ImportCountType::Function);
        if let Some(code_section) = module.code_section_mut() {
            for (func_idx, func_body) in code_section.bodies_mut().iter_mut().enumerate() {
                inject_profiling_prints(print_profiling_fn, (n_fun_imports + func_idx) as u32, func_body);
            }
        }
    }

    assert!(module.has_names_section());
    let names_section = module.names_section().expect("names section").clone();
    let mut mbuilder = builder::from_module(module);
    mbuilder = mbuilder.with_section(Section::Name(names_section));

    let mut instrs = vec![];

    // We need some memory for fd_write or debug_print, so save the content of that
    // in locals. We use the same layout for fd_write and debug_print, for simplicity.
    // Layout:
    // #00 (u32): always #08 (location of data)
    // #04 (u32): always #30 (size of data)
    // #08 (u32): "<pRf" (marker bytes)
    // #12 (u32, base16alpha encoded): function index
    // #20 (u64, base16alpha encoded): cycle counter
    // #36 (u32): ">\n" (marker bytes)
    // Total size: 38, so can be stored in five i64 local

    // store memory
    for i in 0..5 {
        instrs.extend_from_slice(&[
            // storing memory
            Instruction::I32Const(0),
            Instruction::I64Load(3, 8 * i),
            Instruction::SetLocal(1 + i),
        ]);
    }

    // set constant memory parts
    if ! for_ic {
        instrs.extend_from_slice(&[
            Instruction::I32Const(0),
            Instruction::I32Const(8),
            Instruction::I32Store(2, 0),
            Instruction::I32Const(0),
            Instruction::I32Const(30),
            Instruction::I32Store(2, 4),
        ])
    }
    instrs.extend_from_slice(&[
        Instruction::I32Const(0),
        Instruction::I32Const(0x6652703C),
        Instruction::I32Store(2, 8),
        Instruction::I32Const(0),
        Instruction::I32Const(0x0A3E),
        Instruction::I32Store16(0, 36),
    ]);

    // encode function id in base16alpha
    for i in (0 as u8)..8 {
        instrs.extend_from_slice(&[
            Instruction::I32Const(0),
            Instruction::GetLocal(0),
            Instruction::I32Const(i as i32 * 4),
            Instruction::I32ShrU,
            Instruction::I32Const(0xf),
            Instruction::I32And,
            Instruction::I32Const(0x41),
            Instruction::I32Add,
            Instruction::I32Store8(0, 12 + i as u32),
        ]);
    }

    // encode cycle counter in base16alpha
    for i in 0..16 {
        instrs.extend_from_slice(&[
            Instruction::I32Const(0),
            Instruction::GetGlobal(cycles_counter_ix),
            Instruction::I64Const(i * 4),
            Instruction::I64ShrU,
            Instruction::I64Const(0xf),
            Instruction::I64And,
            Instruction::I64Const(0x41),
            Instruction::I64Add,
            Instruction::I64Store8(0, 20 + i as u32),
        ]);
    }

    if for_ic  {
        // call debug_print
        instrs.extend_from_slice(&[
            Instruction::I32Const(8),  // location of data
            Instruction::I32Const(29), // size of data (without \n)
            Instruction::Call(0),      // call debug_print
        ])
    } else {
        // call fd_write
        instrs.extend_from_slice(&[
            Instruction::I32Const(1),  // stderr
            Instruction::I32Const(0),  // iovec ptr
            Instruction::I32Const(1),  // one entry
            Instruction::I32Const(20), // bytes written (we dont care)
            Instruction::Call(0),      // call fd_write
            Instruction::Drop,
        ])
    }

    // store memory
    for i in 0..5 {
        instrs.extend_from_slice(&[
            // storing memory
            Instruction::I32Const(0),
            Instruction::GetLocal(1 + i),
            Instruction::I64Store(3, 8 * i),
        ]);
    }

    instrs.extend_from_slice(&[
            Instruction::End
    ]);

    // push profiling event printer
    mbuilder.push_function(
        builder::function()
            .with_signature(builder::signature().with_param(ValueType::I32).build_sig())
            .body()
            .with_locals(vec![Local::new(5, ValueType::I64)])
            .with_instructions(Instructions::new(instrs))
            .build()
            .build());

    // push the instruction counter
    let module = mbuilder
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I64, true),
            InitExpr::new(vec![Instruction::I64Const(0), Instruction::End]),
        ))
        .build();

    let result = parity_wasm::serialize(module)?;

    Ok(result)
}

// Represents a hint about the context of each basic code block in Wasm.
#[derive(Copy, Clone, Debug, PartialEq)]
enum Scope {
    ReentrantBlockStart,
    NonReentrantBlockStart,
    BlockEnd,
}

// Represents a instruction metering injection point.
#[derive(Copy, Clone, Debug)]
struct InjectionPoint {
    scope: Scope,
    position: usize,
    cost: u64,
}

impl InjectionPoint {
    fn new(position: usize, scope: Scope) -> Self {
        InjectionPoint {
            scope,
            position,
            cost: 0,
        }
    }
}

fn inject_metering(
    code: &mut Instructions,
    instruction_cost_table: &InstructionCostTable,
    cycles_counter_ix: u32,
) {
    let points = injections(code.elements(), instruction_cost_table);
    let points = points.iter().filter(|point| point.cost > 0);
    let orig_elems = code.elements();
    let mut elems: Vec<Instruction> = Vec::new();
    let mut last_injection_position = 0;
    for point in points {
        elems.extend_from_slice(&orig_elems[last_injection_position..point.position]);
        elems.extend_from_slice(&[
            Instruction::GetGlobal(cycles_counter_ix),
            Instruction::I64Const(point.cost as i64),
            Instruction::I64Add,
            Instruction::SetGlobal(cycles_counter_ix),
        ]);
        last_injection_position = point.position;
    }
    elems.extend_from_slice(&orig_elems[last_injection_position..]);
    *code.elements_mut() = elems;
}

fn inject_profiling_prints(print_profiling_fn: u32, func_idx: u32, func_body: &mut FuncBody) {
    let elems = func_body.code_mut().elements_mut();
    let mut tail = elems.split_off(0);
    elems.extend_from_slice(&[
        Instruction::I32Const(func_idx as i32),
        Instruction::Call(print_profiling_fn),
    ]);
    while tail.len() > 1 {
        let i = tail.remove(0);
        if matches!(i, Instruction::Return) {
            elems.extend_from_slice(&[
                Instruction::I32Const(-1),
                Instruction::Call(print_profiling_fn),
            ]);
        }
        elems.extend_from_slice(&[i]);
    }
    assert!(matches!(tail.first().unwrap(), Instruction::End));
    elems.extend_from_slice(&[
        Instruction::I32Const(-1),
        Instruction::Call(print_profiling_fn),
    ]);
    elems.extend_from_slice(&mut tail);
}

// This function scans through the Wasm code and creates an injection point
// at the beginning of every basic block (straight-line sequence of instructions
// with no branches). An injection point contains a "hint" about the context
// of every basic block, specifically if it's re-entrant or not.
fn injections(
    code: &[Instruction],
    instruction_cost_table: &InstructionCostTable,
) -> Vec<InjectionPoint> {
    let mut res = Vec::new();
    let mut stack = Vec::new();
    use Instruction::*;
    let mut curr = InjectionPoint::new(0, Scope::ReentrantBlockStart);
    for (position, i) in code.iter().enumerate() {
        curr.cost += instruction_cost_table.cost(i);
        match i {
            // Start of a re-entrant code block.
            Loop(_) => {
                stack.push(curr);
                curr = InjectionPoint::new(position + 1, Scope::ReentrantBlockStart);
            }
            // Start of a non re-entrant code block.
            If(_) | Block(_) => {
                stack.push(curr);
                curr = InjectionPoint::new(position + 1, Scope::NonReentrantBlockStart);
            }
            // End of a code block but still more code left.
            Else | Br(_) | BrIf(_) | BrTable(_) => {
                res.push(curr);
                curr = InjectionPoint::new(position + 1, Scope::BlockEnd);
            }
            // `End` signals the end of a code block. If there's nothing more on the stack, we've
            // gone through all the code.
            End => {
                res.push(curr);
                curr = match stack.pop() {
                    Some(val) => val,
                    None => break,
                };
            }
            // Nothing special to be done for other instructions.
            _ => (),
        }
    }
    res.sort_by_key(|k| k.position);
    res
}
