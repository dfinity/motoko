use clap::arg_enum;
use std::error::Error;
use structopt::StructOpt;
use wasm_profiler::instrumentation::{instrument, InstructionCostTable};

arg_enum! {
    #[derive(Debug)]
    enum SystemAPI { WASI, IC }
}

#[derive(StructOpt)]
#[structopt(name = "wasm-profiler-instrument", no_version)]

/// This programs instruments wasm programs for instruction profiling.
///
/// Concretely, it inject a global to count down instructions (from 0), according to a weighted cost table.
/// At each function entry and exit, it prints the function id and the current value of the
/// counter.
///
/// The printing can either use the WASI interface (`fd_write` to stdout), or the Internet
/// Comuter interface (`ic0.debug_print`).
///
/// The separate script `wasm-profiler-postproc.pl` can be used to process that output and produce
/// FlameGraph and KCacheGrind compatible profiles.
struct CliArgs {
    /// Which system api to use to print profiling events
    #[structopt(long)]
    wasi_system_api: bool,

    #[structopt(long)]
    ic_system_api: bool,

    #[structopt(short, long)]
    input: String,

    #[structopt(short, long)]
    output: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CliArgs::from_args();

    if args.wasi_system_api && args.ic_system_api {
        Err("Cannot use both --wasi-system-api and --ic-system-api")?;
    }
    let for_ic = args.ic_system_api;

    let contents = std::fs::read(args.input)?;
    let binary = instrument(&contents, for_ic, &InstructionCostTable::default())?;
    std::fs::write(args.output, binary.as_slice())?;
    Ok(())
}
