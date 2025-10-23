use anyhow::{Context, Result};
use clap::{Args, Parser};
use std::{
    fs,
    path::PathBuf,
};
use wac_cli::PackageResolver;
use wac_graph::EncodeOptions;
use wac_graph::types::BorrowedPackageKey;
use wac_parser::Document;

fn version() -> &'static str {
    "0.1.0"
}

fn parse<T, U>(s: &str) -> Result<(T, U)>
where
    T: std::str::FromStr,
    T::Err: Into<anyhow::Error>,
    U: std::str::FromStr,
    U::Err: Into<anyhow::Error>,
{
    let (k, v) = s.split_once('=').context("value does not contain `=`")?;

    Ok((
        k.trim().parse().map_err(Into::into)?,
        v.trim().parse().map_err(Into::into)?,
    ))
}

fn embed_wit(wasm_module: &mut Vec<u8>, wit_file: &PathBuf) -> Result<()> {
    let wit_resolve = wasm_tools::wit::WitResolve {
        wit: wit_file.clone(),
        features: Default::default(),
        all_features: false,
    };
    let (resolve, pkg_id) = wit_resolve.load()?;
    let world = resolve.select_world(&[pkg_id], None)?;
    wit_component::embed_component_metadata(
        wasm_module,
        &resolve,
        world,
        wit_component::StringEncoding::UTF8,
    )
}

fn has_component_type_embedded(wasm_module: &mut Vec<u8>) -> Result<bool> {
    let parser = wasmparser::Parser::new(0);
    let mut section_count = 0;

    for payload in parser.parse_all(wasm_module) {
        match payload? {
            wasmparser::Payload::CustomSection(reader)=> {
                println!("Custom section #{}: {}", section_count, reader.name());
                section_count += 1;
                if reader.name() == "component-type" {
                    return Ok(true);
                }
            }
            _ => continue,
        }
    }
    Ok(false)
}
fn create_motoko_component(wasm_module: &Vec<u8>) -> Result<Vec<u8>> {
    let mut encoder = wit_component::ComponentEncoder::default()
        .validate(true)
        .reject_legacy_names(false);
    encoder = encoder.merge_imports_based_on_semver(true);
    encoder = encoder.module(wasm_module)?;

    let wasi_adapter_wasm = include_bytes!("data/wasi-adapter.wasm");
    encoder = encoder.adapter("wasi_snapshot_preview1", wasi_adapter_wasm)?;
    encoder = encoder.realloc_via_memory_grow(false); // `true` causes a trap...

    let bytes = encoder
        .encode()
        .context("failed to encode a component from module")?;
    Ok(bytes)
}

/// Compose WebAssembly components using the provided WAC source file.
#[derive(Args)]
#[clap(disable_version_flag = true)]
pub struct ComposeCommand {
    /// Paths to package dependencies.
    #[clap(long = "dep", short, value_name = "PKG=PATH", value_parser = parse::<String, PathBuf>)]
    pub deps: Vec<(String, PathBuf)>,

    /// The path to the Motoko Wasm module file.
    #[clap(long, value_name = "PATH")]
    pub wasm_module_file: PathBuf,

    /// The path to the source WIT file.
    #[clap(long, value_name = "PATH")]
    pub wit_file: PathBuf,

    /// The path to the source WAC file.
    #[clap(long, value_name = "PATH")]
    pub wac_file: PathBuf,

    /// The path to write the output to.
    ///
    /// If not specified, the output will be written to stdout.
    #[clap(long, short = 'o')]
    pub output: PathBuf,
}

impl ComposeCommand {
    /// Executes the command.
    pub async fn exec(self) -> Result<()> {
        log::info!("... executing compose command...");

        let mut wasm_module = fs::read(&self.wasm_module_file).with_context(|| {
            format!(
                "failed to read file `{path}`",
                path = self.wasm_module_file.display()
            )
        })?;
        if !has_component_type_embedded(&mut wasm_module)? {
            log::info!("--- component_type is not embedded, embedding it now...");
            embed_wit(&mut wasm_module, &self.wit_file)?;
            log::info!("--- done embedding WIT.");
        } else {
            log::info!("--- component_type is already embedded, skipping embed operation");
        }

        let wasm_component = create_motoko_component(&wasm_module)?;
        log::info!("--- done creating core Motoko component.");

        let wac_contents = fs::read_to_string(&self.wac_file).with_context(|| {
            format!(
                "failed to read file `{path}`",
                path = self.wac_file.display()
            )
        })?;

        let wac_document = Document::parse(&wac_contents)?;
        log::info!("--- using the following dependencies:");
        for (dep, path) in self.deps.iter() {
            log::info!("    {} -- {}.", dep, path.display());
        }
        // deps-map for the resolver are pairs (dep, path-to-file).
        // To avoid writing `wasm_component` to a file, that will be read immediately after writing,
        // we first create an entry for `motoko:component` in the deps-map, wrongly mapping
        // to the wasm_module_file (so that all packages mentioned in wac-file are present),
        // and later in the resulting `packages` we set the value to the correct `wasm_component`.
        let mut mut_deps = self.deps.clone();
        mut_deps.push((
            "motoko:component".parse()?,
            PathBuf::from(&self.wasm_module_file),
        ));
        // We do not use deps-dir, as it expects a specific data layout, incompatible
        // with mops' conventions.
        let mut resolver =
            PackageResolver::new("no_deps_dir", mut_deps.into_iter().collect()).await?;
        let mut packages = resolver.resolve(&wac_document).await?;
        // here we set the right value for the core package "motoko:component"
        packages.insert(
            BorrowedPackageKey::from_name_and_version("motoko:component", None),
            wasm_component,
        );

        let resolution = wac_document.resolve(packages)?;
        let bytes = resolution.encode(EncodeOptions {
            define_components: true,
            validate: true,
            ..Default::default()
        })?;

        fs::write(&self.output, bytes).context(format!(
            "failed to write output file `{path}`",
            path = self.output.display()
        ))?;
        log::info!(
            "--- done writing combined Wasm to {path}.",
            path = self.output.display()
        );

        Ok(())
    }
}

/// Tool for converting Wasm module that uses components to a composed Wasm component.
#[derive(Parser)]
#[clap(
    bin_name = "mo2wc",
    version,
    propagate_version = true,
    arg_required_else_help = true
)]
#[command(version = version())]
enum Mo2wc {
    Compose(ComposeCommand),
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    if let Err(e) = match Mo2wc::parse() {
        Mo2wc::Compose(cmd) => cmd.exec().await,
    } {
        eprintln!("error: {e:?}");
        std::process::exit(1);
    }

    Ok(())
}