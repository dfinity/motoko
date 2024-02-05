use parity_wasm::builder;
use parity_wasm::elements::{
    BulkInstruction, FuncBody, GlobalEntry, GlobalType, ImportCountType, InitExpr, Instruction,
    Instructions, Internal, Local, Module, Section, SignExtInstruction, ValueType,
};
use std::error::Error;
use std::iter::FromIterator;

use crate::instrumentation::ic_calls::{FunctionCost, InjectionKind};

mod ic_calls;

// Adopted from IC code with slight adjustments.
// This implements the IC's new instruction cost function:
// https://github.com/dfinity/ic/blob/d49f4daea38ca25fe61012214e049ecc0866292d/rs/embedders/src/wasm_utils/instrumentation.rs#L174
pub fn instruction_to_cost_new(i: &Instruction) -> u64 {
    // This aims to be a complete list of all instructions that can be executed, with certain exceptions.
    // The exceptions are: SIMD instructions, atomic instructions, and the dynamic cost of
    // of operations such as table/memory fill, copy, init. This
    // dynamic cost is treated separately. Here we only assign a static cost to these instructions.
    match i {
        // The following instructions are mostly signaling the start/end of code blocks,
        // so we assign 0 cost to them.
        Instruction::Block { .. } => 0,
        Instruction::Else => 0,
        Instruction::End => 0,
        Instruction::Loop { .. } => 0,

        // The following instructions generate register/immediate code most of the time,
        // so we assign 1 cost to them because these are not very costly to execute,
        // they simply take out resources (registers or instr cache).
        Instruction::I32Const { .. }
        | Instruction::I64Const { .. }
        | Instruction::F32Const { .. }
        | Instruction::F64Const { .. } => 1,

        // All integer arithmetic instructions (32 bit and 64 bit) are of cost 1 with the
        // exception of division and remainder instructions, which are of cost 10. Validated
        // in benchmarks.
        Instruction::I32Add { .. }
        | Instruction::I32Sub { .. }
        | Instruction::I32Mul { .. }
        | Instruction::I32And { .. }
        | Instruction::I32Or { .. }
        | Instruction::I32Xor { .. }
        | Instruction::I32Shl { .. }
        | Instruction::I32ShrS { .. }
        | Instruction::I32ShrU { .. }
        | Instruction::I32Rotl { .. }
        | Instruction::I32Rotr { .. }
        | Instruction::I64Add { .. }
        | Instruction::I64Sub { .. }
        | Instruction::I64Mul { .. }
        | Instruction::I64And { .. }
        | Instruction::I64Or { .. }
        | Instruction::I64Xor { .. }
        | Instruction::I64Shl { .. }
        | Instruction::I64ShrS { .. }
        | Instruction::I64ShrU { .. }
        | Instruction::I64Rotl { .. }
        | Instruction::I64Rotr { .. } => 1,

        Instruction::I32DivS { .. }
        | Instruction::I32DivU { .. }
        | Instruction::I32RemS { .. }
        | Instruction::I32RemU { .. }
        | Instruction::I64DivS { .. }
        | Instruction::I64DivU { .. }
        | Instruction::I64RemS { .. }
        | Instruction::I64RemU { .. } => 10,

        // All integer (32 and 64 bit) comparison operations are of cost 1.
        // That is because they boil down to simple arithmetic operations, which are also
        // of cost 1. Validated in Benchmarks.
        Instruction::I32Eqz { .. }
        | Instruction::I32Eq { .. }
        | Instruction::I32Ne { .. }
        | Instruction::I32LtS { .. }
        | Instruction::I32LtU { .. }
        | Instruction::I32GtS { .. }
        | Instruction::I32GtU { .. }
        | Instruction::I32LeS { .. }
        | Instruction::I32LeU { .. }
        | Instruction::I32GeS { .. }
        | Instruction::I32GeU { .. }
        | Instruction::I64Eqz { .. }
        | Instruction::I64Eq { .. }
        | Instruction::I64Ne { .. }
        | Instruction::I64LtS { .. }
        | Instruction::I64LtU { .. }
        | Instruction::I64GtS { .. }
        | Instruction::I64GtU { .. }
        | Instruction::I64LeS { .. }
        | Instruction::I64LeU { .. }
        | Instruction::I64GeS { .. }
        | Instruction::I64GeU { .. } => 1,

        // All floating point instructions (32 and 64 bit) are of cost 50 because they are expensive CPU operations.
        //The exception is neg, abs, and copysign, which are cost 2, as they are more efficient.
        // Comparing floats is cost 1. Validated in Benchmarks.
        // The cost is adjusted to 20 after benchmarking with real canisters.
        Instruction::F32Add { .. }
        | Instruction::F32Sub { .. }
        | Instruction::F32Mul { .. }
        | Instruction::F32Div { .. }
        | Instruction::F32Min { .. }
        | Instruction::F32Max { .. }
        | Instruction::F32Ceil { .. }
        | Instruction::F32Floor { .. }
        | Instruction::F32Trunc { .. }
        | Instruction::F32Nearest { .. }
        | Instruction::F32Sqrt { .. }
        | Instruction::F64Add { .. }
        | Instruction::F64Sub { .. }
        | Instruction::F64Mul { .. }
        | Instruction::F64Div { .. }
        | Instruction::F64Min { .. }
        | Instruction::F64Max { .. }
        | Instruction::F64Ceil { .. }
        | Instruction::F64Floor { .. }
        | Instruction::F64Trunc { .. }
        | Instruction::F64Nearest { .. }
        | Instruction::F64Sqrt { .. } => 20,

        Instruction::F32Abs { .. }
        | Instruction::F32Neg { .. }
        | Instruction::F32Copysign { .. }
        | Instruction::F64Abs { .. }
        | Instruction::F64Neg { .. }
        | Instruction::F64Copysign { .. } => 2,

        // Comparison operations for floats are of cost 3 because they are usually implemented
        // as arithmetic operations on integers (the individual components, sign, exp, mantissa,
        // see https://en.wikipedia.org/wiki/Floating-point_arithmetic#Comparison).
        // Validated in benchmarks.
        Instruction::F32Eq { .. }
        | Instruction::F32Ne { .. }
        | Instruction::F32Lt { .. }
        | Instruction::F32Gt { .. }
        | Instruction::F32Le { .. }
        | Instruction::F32Ge { .. }
        | Instruction::F64Eq { .. }
        | Instruction::F64Ne { .. }
        | Instruction::F64Lt { .. }
        | Instruction::F64Gt { .. }
        | Instruction::F64Le { .. }
        | Instruction::F64Ge { .. } => 3,

        // All Extend instructions are of cost 1.
        Instruction::I32WrapI64 { .. }
        | Instruction::SignExt(SignExtInstruction::I32Extend8S { .. })
        | Instruction::SignExt(SignExtInstruction::I32Extend16S { .. })
        | Instruction::SignExt(SignExtInstruction::I64Extend8S { .. })
        | Instruction::SignExt(SignExtInstruction::I64Extend16S { .. })
        | Instruction::SignExt(SignExtInstruction::I64Extend32S { .. })
        | Instruction::F64ReinterpretI64 { .. }
        | Instruction::I64ReinterpretF64 { .. }
        | Instruction::I32ReinterpretF32 { .. }
        | Instruction::F32ReinterpretI32 { .. }
        | Instruction::I64ExtendSI32 { .. }
        | Instruction::I64ExtendUI32 { .. } => 1,

        // Convert to signed is cheaper than converting to unsigned, validated in benchmarks.
        Instruction::F32ConvertSI32 { .. }
        | Instruction::F64ConvertSI64 { .. }
        | Instruction::F32ConvertSI64 { .. }
        | Instruction::F64ConvertSI32 { .. } => 3,

        Instruction::F64ConvertUI32 { .. }
        | Instruction::F32ConvertUI64 { .. }
        | Instruction::F32ConvertUI32 { .. }
        | Instruction::F64ConvertUI64 { .. } => 16,

        // TruncSat ops are expensive because of floating point manipulation. Cost is 50,
        // validated in benchmarks.
        // The cost is adjusted to 20 after benchmarking with real canisters.
        // Not supported by `parity_wasm`

        // Promote and demote are of cost 1.
        Instruction::F32DemoteF64 { .. } | Instruction::F64PromoteF32 { .. } => 1,

        // Trunc ops are expensive because of floating point manipulation. Cost is 30, validated in benchmarks.
        // The cost is adjusted to 20 after benchmarking with real canisters.
        Instruction::I32TruncSF32 { .. }
        | Instruction::I32TruncUF32 { .. }
        | Instruction::I32TruncSF64 { .. }
        | Instruction::I32TruncUF64 { .. }
        | Instruction::I64TruncSF32 { .. }
        | Instruction::I64TruncUF32 { .. }
        | Instruction::I64TruncSF64 { .. }
        | Instruction::I64TruncUF64 { .. } => 20,

        // All load/store instructions are of cost 2.
        // Validated in benchmarks.
        // The cost is adjusted to 1 after benchmarking with real canisters.
        Instruction::I32Load { .. }
        | Instruction::I64Load { .. }
        | Instruction::F32Load { .. }
        | Instruction::F64Load { .. }
        | Instruction::I32Load8S { .. }
        | Instruction::I32Load8U { .. }
        | Instruction::I32Load16S { .. }
        | Instruction::I32Load16U { .. }
        | Instruction::I64Load8S { .. }
        | Instruction::I64Load8U { .. }
        | Instruction::I64Load16S { .. }
        | Instruction::I64Load16U { .. }
        | Instruction::I64Load32S { .. }
        | Instruction::I64Load32U { .. }
        | Instruction::I32Store { .. }
        | Instruction::I64Store { .. }
        | Instruction::F32Store { .. }
        | Instruction::F64Store { .. }
        | Instruction::I32Store8 { .. }
        | Instruction::I32Store16 { .. }
        | Instruction::I64Store8 { .. }
        | Instruction::I64Store16 { .. }
        | Instruction::I64Store32 { .. } => 1,

        // Global get/set operations are similarly expensive to loads/stores.
        Instruction::GetGlobal { .. } | Instruction::SetGlobal { .. } => 2,

        // TableGet and TableSet are expensive operations because they
        // are translated into memory manipulation operations.
        // Results based on benchmarks. Costs 5.
        // Not supported in `parity_wasm`.

        // LocalGet and LocalSet, LocalTee and Select are of cost 1.
        // In principle, they should be equivalent to load/store (cost 2), but they perform load/store
        // from the stack, which is "nearby" memory, which is likely to be in the cache.
        Instruction::GetLocal { .. }
        | Instruction::SetLocal { .. }
        | Instruction::TeeLocal { .. }
        | Instruction::Select { .. } => 1,

        // Memory Grow and Table Grow Size expensive operations because they call
        // into the system, hence their cost is 300. Memory Size and Table Size are
        // cheaper, their cost is 20. Results validated in benchmarks.
        // Missing in `parity_wasm`: TableGrow => 300, TableSize => 100.
        Instruction::GrowMemory { .. } => 300,
        Instruction::CurrentMemory { .. } => 20,

        // Bulk memory ops are of cost 100. They are heavy operations because
        // they are translated into function calls in the x86 disassembly. Validated
        // in benchmarks.
        // Missing in `parity_wasm`: TableFill => 100.
        Instruction::Bulk(BulkInstruction::MemoryFill { .. })
        | Instruction::Bulk(BulkInstruction::MemoryCopy { .. })
        | Instruction::Bulk(BulkInstruction::TableCopy { .. })
        | Instruction::Bulk(BulkInstruction::MemoryInit { .. })
        | Instruction::Bulk(BulkInstruction::TableInit { .. }) => 100,

        // DataDrop (=MemoryDrop) and Elem drop (=TableDrop) are of cost 300.
        Instruction::Bulk(BulkInstruction::TableDrop { .. })
        | Instruction::Bulk(BulkInstruction::MemoryDrop { .. }) => 300,

        // Call instructions are of cost 20. Validated in benchmarks.
        // The cost is adjusted to 5 and 10 after benchmarking with real canisters.
        Instruction::Call { .. } => 5,
        Instruction::CallIndirect { .. } => 10,

        // Return, drop, unreachable and nop instructions are of cost 1.
        Instruction::Return { .. }
        | Instruction::Drop
        | Instruction::Unreachable
        | Instruction::Nop => 1,

        // Branching instructions should be of cost 2.
        Instruction::If { .. }
        | Instruction::Br { .. }
        | Instruction::BrIf { .. }
        | Instruction::BrTable { .. } => 2,

        // Popcnt and Clz instructions are cost 1. Validated in benchmarks.
        Instruction::I32Popcnt { .. }
        | Instruction::I64Popcnt { .. }
        | Instruction::I32Clz { .. }
        | Instruction::I32Ctz { .. }
        | Instruction::I64Clz { .. }
        | Instruction::I64Ctz { .. } => 1,
        // Reference instructions are not supported in `parity_wasm`:
        // RefNull => 1, RefIsNull => 5, RefFunc => 130,
    }
}

fn inject_imports(for_ic: bool, module: Module) -> Module {
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
                .module("wasi_snapshot_preview1")
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

struct SpecialIndices {
    cycles_counter_ix: u32,
    print_profiling_fn: u32,
    dynamic_counter_fn: u32,
    dynamic_counter64_fn: u32,
}

/// Takes a Wasm binary and inserts the instruction metering and profiling code.
/// Returns  the instrumented binary.
pub fn instrument(wasm: &Vec<u8>, for_ic: bool) -> Result<Vec<u8>, Box<dyn Error>> {
    let module = parity_wasm::deserialize_buffer::<Module>(wasm.as_slice())?;
    let module = module
        .parse_names()
        .map_err(|(mut es, _)| es.pop().unwrap().1)?;
    assert!(module.has_names_section());
    let mut module = inject_imports(for_ic, module);
    assert!(module.has_names_section());
    let num_functions = module.functions_space() as u32;
    let num_globals = module.globals_space() as u32;

    let special_indices = SpecialIndices {
        cycles_counter_ix: num_globals,
        print_profiling_fn: num_functions,
        dynamic_counter_fn: num_functions + 1,
        dynamic_counter64_fn: num_functions + 2,
    };

    let ic_call_costs = FunctionCost::new(&module);

    // inject cycles counter decrementation
    {
        if let Some(code_section) = module.code_section_mut() {
            for func_body in code_section.bodies_mut().iter_mut() {
                let code = func_body.code_mut();
                inject_metering(code, &special_indices, &ic_call_costs);
            }
        }
    }

    // inject printing of counter
    {
        let n_fun_imports = module.import_count(ImportCountType::Function);
        if let Some(code_section) = module.code_section_mut() {
            for (func_idx, func_body) in code_section.bodies_mut().iter_mut().enumerate() {
                inject_profiling_prints(
                    special_indices.print_profiling_fn,
                    (n_fun_imports + func_idx) as u32,
                    func_body,
                );
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
    if !for_ic {
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
    for i in 0u8..8 {
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
            Instruction::GetGlobal(special_indices.cycles_counter_ix),
            Instruction::I64Const(i * 4),
            Instruction::I64ShrU,
            Instruction::I64Const(0xf),
            Instruction::I64And,
            Instruction::I64Const(0x41),
            Instruction::I64Add,
            Instruction::I64Store8(0, 20 + i as u32),
        ]);
    }

    if for_ic {
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

    instrs.extend_from_slice(&[Instruction::End]);

    // push profiling event printer
    mbuilder.push_function(
        builder::function()
            .with_signature(builder::signature().with_param(ValueType::I32).build_sig())
            .body()
            .with_locals(vec![Local::new(5, ValueType::I64)])
            .with_instructions(Instructions::new(instrs))
            .build()
            .build(),
    );

    // push dynamic instruction counter function (32-bit increment)
    mbuilder.push_function(
        builder::function()
            .with_signature(
                builder::signature()
                    .with_param(ValueType::I32)
                    .with_result(ValueType::I32)
                    .build_sig(),
            )
            .body()
            .with_instructions(Instructions::new(vec![
                Instruction::GetLocal(0),
                Instruction::I64ExtendUI32,
                Instruction::GetGlobal(special_indices.cycles_counter_ix),
                Instruction::I64Add,
                Instruction::SetGlobal(special_indices.cycles_counter_ix),
                Instruction::GetLocal(0),
                Instruction::End,
            ]))
            .build()
            .build(),
    );

    // push dynamic instruction counter function (64-bit increment)
    mbuilder.push_function(
        builder::function()
            .with_signature(
                builder::signature()
                    .with_param(ValueType::I64)
                    .with_result(ValueType::I64)
                    .build_sig(),
            )
            .body()
            .with_instructions(Instructions::new(vec![
                Instruction::GetLocal(0),
                Instruction::GetGlobal(special_indices.cycles_counter_ix),
                Instruction::I64Add,
                Instruction::SetGlobal(special_indices.cycles_counter_ix),
                Instruction::GetLocal(0),
                Instruction::End,
            ]))
            .build()
            .build(),
    );

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

#[derive(Copy, Clone, Debug)]
struct InjectionPoint {
    position: usize,
    cost: u64,
    kind: InjectionKind,
}

impl InjectionPoint {
    fn new_static_cost(position: usize, cost: u64) -> Self {
        InjectionPoint {
            position,
            cost,
            kind: InjectionKind::Static,
        }
    }

    fn new_dynamic_cost(position: usize, kind: InjectionKind) -> Self {
        InjectionPoint {
            position,
            cost: 0,
            kind,
        }
    }
}

fn inject_metering(
    code: &mut Instructions,
    special_indices: &SpecialIndices,
    ic_call_costs: &FunctionCost,
) {
    let points = injections_new(code.elements(), ic_call_costs);
    let points = points.iter().filter(|point| match point.kind {
        InjectionKind::Static => point.cost > 0,
        InjectionKind::Dynamic | InjectionKind::Dynamic64 => true,
    });
    let orig_elems = code.elements();
    let mut elems: Vec<Instruction> = Vec::new();
    let mut last_injection_position = 0;
    for point in points {
        elems.extend_from_slice(&orig_elems[last_injection_position..point.position]);
        match point.kind {
            InjectionKind::Static => {
                elems.extend_from_slice(&[
                    Instruction::GetGlobal(special_indices.cycles_counter_ix),
                    Instruction::I64Const(point.cost as i64),
                    Instruction::I64Add,
                    Instruction::SetGlobal(special_indices.cycles_counter_ix),
                ]);
            }
            InjectionKind::Dynamic => {
                elems.extend_from_slice(&[Instruction::Call(special_indices.dynamic_counter_fn)]);
            }
            InjectionKind::Dynamic64 => {
                elems.extend_from_slice(&[Instruction::Call(special_indices.dynamic_counter64_fn)]);
            }
        }
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
    elems.extend_from_slice(&tail);
}

// Source: https://github.com/dfinity/ic/blob/d49f4daea38ca25fe61012214e049ecc0866292d/rs/embedders/src/wasm_utils/instrumentation.rs#L1575
// With slight adjustments.
// This function scans through the Wasm code and creates an injection point
// at the beginning of every basic block (straight-line sequence of instructions
// with no branches) and before each bulk memory instruction. An injection point
// contains a "hint" about the context of every basic block, specifically if
// it's re-entrant or not.
fn injections_new(code: &[Instruction], ic_call_costs: &FunctionCost) -> Vec<InjectionPoint> {
    let mut res = Vec::new();
    use Instruction::*;
    // The function itself is a re-entrant code block.
    // Start with at least one fuel being consumed because even empty
    // functions should consume at least some fuel.
    let mut curr = InjectionPoint::new_static_cost(0, 1);
    for (position, i) in code.iter().enumerate() {
        curr.cost += instruction_to_cost_new(i);
        match i {
            // Start of a re-entrant code block.
            Loop { .. } => {
                res.push(curr);
                curr = InjectionPoint::new_static_cost(position + 1, 0);
            }
            // Start of a non re-entrant code block.
            If { .. } => {
                res.push(curr);
                curr = InjectionPoint::new_static_cost(position + 1, 0);
            }
            // End of a code block but still more code left.
            Else | Br { .. } | BrIf { .. } | BrTable { .. } => {
                res.push(curr);
                curr = InjectionPoint::new_static_cost(position + 1, 0);
            }
            End => {
                res.push(curr);
                curr = InjectionPoint::new_static_cost(position + 1, 0);
            }
            // ReturnCall and ReturnCallIndirect are not supported by `parity_wasm`.
            Return | Unreachable => {
                res.push(curr);
                // This injection point will be unreachable itself (most likely empty)
                // but we create it to keep the algorithm uniform
                curr = InjectionPoint::new_static_cost(position + 1, 0);
            }
            // Bulk memory instructions require injected metering __before__ the instruction
            // executes so that size arguments can be read from the stack at runtime.
            // TableFill is not supported by `parity_wasm`.
            Bulk(BulkInstruction::MemoryFill { .. })
            | Bulk(BulkInstruction::MemoryCopy { .. })
            | Bulk(BulkInstruction::MemoryInit { .. })
            | Bulk(BulkInstruction::TableCopy { .. })
            | Bulk(BulkInstruction::TableInit { .. }) => {
                res.push(InjectionPoint::new_dynamic_cost(
                    position,
                    InjectionKind::Dynamic,
                ));
            }
            // Count additional IC call costs if applicable.
            // Source: https://github.com/dfinity/ic-wasm/blob/61692f44cf85b93d43311492283246bb443449d3/src/instrumentation.rs#L200
            // With slight adjustments.
            Call(function_id) => match ic_call_costs.get_cost(*function_id) {
                None => {}
                Some((ic_static_cost, InjectionKind::Static)) => curr.cost += ic_static_cost,
                Some((ic_static_cost, kind @ InjectionKind::Dynamic))
                | Some((ic_static_cost, kind @ InjectionKind::Dynamic64)) => {
                    curr.cost += ic_static_cost;
                    res.push(InjectionPoint::new_dynamic_cost(position, kind));
                }
            },
            // Nothing special to be done for other instructions.
            _ => (),
        }
    }

    res.sort_by_key(|k| k.position);
    res
}
