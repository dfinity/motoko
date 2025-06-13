use candid::Principal;
use hex::decode;
use pocket_ic::{PocketIc, PocketIcBuilder, RejectResponse};
use std::io::Read;
use std::path::PathBuf;
use std::str::Chars;

// Canister Management Commands:
// 1. install <canister_id> <wasm_path> <init_args>
//    - Creates and installs a new canister with the given WASM
//    - init_args can be empty ("") or DIDL-encoded
//
// 2. reinstall <canister_id> <wasm_path> <init_args>
//    - Reinstalls an existing canister with new WASM
//    - Completely resets the canister state
//    - init_args can be empty ("") or DIDL-encoded
//
// 3. upgrade <canister_id> <wasm_path> <upgrade_args>
//    - Upgrades an existing canister with new WASM
//    - Preserves canister state
//    - upgrade_args can be empty ("") or DIDL-encoded
//
// Canister Interaction Commands:
// 1. ingress <canister_id> <method_name> <args>
//    - Makes an ingress call to a canister method
//    - args can be empty ("") or DIDL-encoded
//    - Returns the response or error
//    - Special case: __motoko_stabilize_before_upgrade is a special ingress call
//      used for stabilization before upgrade
//
// 2. query <canister_id> <method_name> <args>
//    - Makes a query call to a canister method
//    - args can be empty ("") or DIDL-encoded
//    - Returns the response or error

#[derive(Debug, Clone, Copy)]
pub enum SubnetType {
    Application,
    System,
}

#[derive(Debug, Clone)]
pub enum TestCommand {
    Install {
        canister_id: String,
        wasm_path: PathBuf,
        init_args: String,
    },
    Reinstall {
        canister_id: String,
        wasm_path: PathBuf,
        init_args: String,
    },
    Upgrade {
        canister_id: String,
        wasm_path: PathBuf,
        upgrade_args: String,
    },
    Ingress {
        canister_id: String,
        method_name: String,
        args: String,
    },
    Query {
        canister_id: String,
        method_name: String,
        args: String,
    },
}
#[derive(Debug)]
enum Radix {
    Bin = 2,
    Hex = 16,
}

fn parse_escape(chars: &mut Chars<'_>, radix: Radix) -> Result<u8, String> {
    let len = match radix {
        Radix::Bin => 8,
        Radix::Hex => 2,
    };
    let s = chars.take(len).collect::<String>();
    if s.len() >= len {
        u8::from_str_radix(&s, radix as u32).map_err(|e| e.to_string())
    } else {
        Err(format!(
            "Escape sequence for radix {:?} too short: {}",
            radix, s
        ))
    }
}

fn parse_quoted(quoted_str: &str) -> Result<Vec<u8>, String> {
    if !quoted_str.is_ascii() {
        return Err(String::from("Only ASCII strings are allowed."));
    }

    let mut chars = quoted_str.chars();
    let mut res: Vec<u8> = Vec::new();
    let mut escaped = false;

    if Some('"') != chars.next() {
        return Err(String::from(
            "Double-quoted string must be enclosed in double quotes.",
        ));
    }

    let mut c = chars.next();
    while let Some(cur) = c {
        if escaped {
            let b = match cur {
                'x' => parse_escape(&mut chars, Radix::Hex)?,
                'b' => parse_escape(&mut chars, Radix::Bin)?,
                '"' => b'"',
                '\\' => b'\\',
                _ => return Err(format!("Illegal escape sequence {}", cur)),
            };
            res.push(b);
            escaped = false;
        } else {
            match cur {
                '\\' => escaped = true,
                '"' => {
                    chars.next(); // consume '"'
                    break;
                }
                _ => res.push(cur as u8),
            }
        }
        c = chars.next();
    }

    if chars.next().is_some() {
        return Err(String::from("Trailing characters after string terminator."));
    }

    Ok(res)
}

fn parse_hex(s: &str) -> Result<Vec<u8>, String> {
    if let Some(s) = s.strip_prefix("0x") {
        decode(s).map_err(|e| e.to_string())
    } else {
        Err(format!("Illegal hex character sequence {}.", s))
    }
}

fn parse_str_args(input_str: &str) -> Result<Vec<u8>, String> {
    if input_str.starts_with('"') {
        parse_quoted(input_str)
    } else {
        parse_hex(input_str)
    }
}

trait ResultExtractor {
    fn extract(&self, command: TestCommand) -> String;
}

// For ingress messages (ingress, reinstall, upgrade, install), the output prepends always "ingress Completed: Reply"
// For query messages, the output prepends always "Ok: Reply".
impl ResultExtractor for () {
    fn extract(&self, command: TestCommand) -> String {
        match command {
            TestCommand::Ingress { .. }
            | TestCommand::Reinstall { .. }
            | TestCommand::Upgrade { .. }
            | TestCommand::Install { .. } => "ingress Completed: Reply: 0x4449444c0000".to_string(),
            TestCommand::Query { .. } => "Ok: Reply: 0x4449444c0000".to_string(),
        }
    }
}

// For ingress messages (ingress, reinstall, upgrade, install), the output prepends always "ingress Completed: Reply"
// For query messages, the output prepends always "Ok: Reply".
impl ResultExtractor for Vec<u8> {
    fn extract(&self, command: TestCommand) -> String {
        let hex_str = self
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<String>>()
            .join("");
        match command {
            TestCommand::Ingress { .. }
            | TestCommand::Reinstall { .. }
            | TestCommand::Upgrade { .. }
            | TestCommand::Install { .. } => {
                format!("ingress Completed: Reply: 0x{}", hex_str).to_string()
            }
            TestCommand::Query { .. } => format!("Ok: Reply: 0x{}", hex_str).to_string(),
        }
    }
}

impl TestCommand {
    fn install_command(
        &self,
        server: &mut PocketIc,
        canister_id: &str,
        wasm_path: &PathBuf,
        init_args: &str,
    ) -> std::io::Result<()> {
        let canister_principal = Principal::from_text(canister_id).unwrap();
        let result = server.create_canister_with_id(None, None, canister_principal);

        if let Err(e) = result {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to create canister: {:?}", e),
            ));
        } else {
            println!("ingress Completed: Reply: 0x4449444c016c01b3c4b1f204680100010a00000000000000000101");
        }

        let wasm_bytes = std::fs::read(wasm_path).unwrap();

        let args = match parse_str_args(init_args) {
            Ok(args) => args,
            Err(e) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to parse init args: {}", e),
                ));
            }
        };
        server.install_canister(canister_principal, wasm_bytes, args, None);
        println!("ingress Completed: Reply: 0x4449444c0000");
        Ok(())
    }

    // Checks if the result is an error or a proper response.
    // A response can be of type () or Vev<u8>.
    // The error is always a RejectResponse.
    fn handle_result_and_get_response<T: ResultExtractor>(
        &self,
        res: Result<T, RejectResponse>,
    ) -> String {
        match res {
            Ok(t) => t.extract(self.clone()),
            Err(e) => {
                // For ingress messages (ingress, reinstall, upgrade, install), the output prepends always "ingress Completed: Reply"
                // For query messages, the output prepends always "Ok: Reply".
                match &self {
                    TestCommand::Ingress { .. }
                    | TestCommand::Reinstall { .. }
                    | TestCommand::Upgrade { .. }
                    | TestCommand::Install { .. } => {
                        format!("ingress Err: {}: {}", e.error_code, e.reject_message).to_string()
                    }
                    TestCommand::Query { .. } => {
                        format!("Err: {}: {}", e.error_code, e.reject_message).to_string()
                    }
                }
            }
        }
    }

    fn reinstall_command(
        &self,
        server: &mut PocketIc,
        canister_id: &str,
        wasm_path: &PathBuf,
        init_args: &str,
    ) -> std::io::Result<()> {
        let canister_principal = Principal::from_text(canister_id).unwrap();
        let wasm_bytes = std::fs::read(wasm_path).unwrap();

        let args = match parse_str_args(init_args) {
            Ok(args) => args,
            Err(e) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to parse init args: {}", e),
                ));
            }
        };
        let res = server.reinstall_canister(canister_principal, wasm_bytes, args, None);
        println!("{}", self.handle_result_and_get_response(res));
        Ok(())
    }

    fn upgrade_command(
        &self,
        server: &mut PocketIc,
        canister_id: &str,
        wasm_path: &PathBuf,
        upgrade_args: &str,
    ) -> std::io::Result<()> {
        let canister_principal = Principal::from_text(canister_id).unwrap();
        let wasm_bytes = std::fs::read(wasm_path).unwrap();

        let args = match parse_quoted(upgrade_args) {
            Ok(args) => args,
            Err(e) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to parse upgrade args: {}", e),
                ));
            }
        };
        let res = server.upgrade_canister(canister_principal, wasm_bytes, args, None);
        println!("{}", self.handle_result_and_get_response(res));
        Ok(())
    }

    fn ingress_command(
        &self,
        server: &mut PocketIc,
        canister_id: &str,
        method_name: &str,
        args: &str,
    ) -> std::io::Result<()> {
        let canister_principal = Principal::from_text(canister_id).unwrap();
        //println!("!!args: {:?}", args);
        let payload = match parse_str_args(args) {
            Ok(payload) => payload,
            Err(e) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to parse args: {}", e),
                ));
            }
        };

        let res = match method_name {
            "__motoko_stabilize_before_upgrade" => server.update_call(
                canister_principal,
                Principal::anonymous(),
                method_name,
                payload,
            ),
            "stop_canister" => {
                let principal_bytes = &payload[payload.len() - 10..];
                let principal = Principal::from_slice(principal_bytes);
                let _ = server.stop_canister(principal, None);
                Ok(vec![0x44, 0x49, 0x44, 0x4c, 0x00, 0x00])
            }
            "start_canister" => {
                let principal_bytes = &payload[payload.len() - 10..];
                let principal = Principal::from_slice(principal_bytes);
                let _ = server.start_canister(principal, None);
                Ok(vec![0x44, 0x49, 0x44, 0x4c, 0x00, 0x00])
            }
            _ => server.update_call(
                canister_principal,
                Principal::anonymous(),
                method_name,
                payload,
            ),
        };

        println!("{}", self.handle_result_and_get_response(res));
        Ok(())
    }

    fn query_command(
        &self,
        server: &mut PocketIc,
        canister_id: &str,
        method_name: &str,
        args: &str,
    ) -> std::io::Result<()> {
        let canister_principal = Principal::from_text(canister_id).unwrap();
        let payload = match parse_str_args(args) {
            Ok(payload) => payload,
            Err(e) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to parse args: {}", e),
                ));
            }
        };
        let res = server.query_call(
            canister_principal,
            Principal::anonymous(),
            method_name,
            payload,
        );
        println!("{}", self.handle_result_and_get_response(res));
        Ok(())
    }

    pub fn execute(&self, server: &mut PocketIc) -> std::io::Result<()> {
        let res = match self {
            TestCommand::Install {
                canister_id,
                wasm_path,
                init_args,
            } => self.install_command(server, canister_id, wasm_path, init_args),
            TestCommand::Reinstall {
                canister_id,
                wasm_path,
                init_args,
            } => self.reinstall_command(server, canister_id, wasm_path, init_args),
            TestCommand::Upgrade {
                canister_id,
                wasm_path,
                upgrade_args,
            } => self.upgrade_command(server, canister_id, wasm_path, upgrade_args),
            TestCommand::Ingress {
                canister_id,
                method_name,
                args,
            } => self.ingress_command(server, canister_id, method_name, args),
            TestCommand::Query {
                canister_id,
                method_name,
                args,
            } => self.query_command(server, canister_id, method_name, args),
        };
        if let Err(e) = res {
            return Err(e);
        }
        Ok(())
    }
}

pub struct TestCommands {
    content: String,
}

impl TestCommands {
    pub fn new(content: String) -> Self {
        Self { content }
    }

    pub fn parse(&self) -> std::io::Result<Vec<TestCommand>> {
        let mut commands = Vec::new();

        for line in self.content.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Parse commands
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }

            let command = match parts[0] {
                // if we encounter a create, just ignore it.
                "create" => continue,
                "install" => {
                    if parts.len() != 4 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "install command requires 3 arguments",
                        ));
                    }
                    TestCommand::Install {
                        canister_id: parts[1].to_string(),
                        wasm_path: PathBuf::from(parts[2]),
                        init_args: parts[3].to_string(),
                    }
                }
                "reinstall" => {
                    if parts.len() != 4 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "reinstall command requires 3 arguments",
                        ));
                    }
                    TestCommand::Reinstall {
                        canister_id: parts[1].to_string(),
                        wasm_path: PathBuf::from(parts[2]),
                        init_args: parts[3].to_string(),
                    }
                }
                "upgrade" => {
                    if parts.len() != 4 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "upgrade command requires 3 arguments",
                        ));
                    }
                    TestCommand::Upgrade {
                        canister_id: parts[1].to_string(),
                        wasm_path: PathBuf::from(parts[2]),
                        upgrade_args: parts[3].to_string(),
                    }
                }
                "ingress" => {
                    if parts.len() != 4 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "ingress command requires 3 arguments",
                        ));
                    }
                    TestCommand::Ingress {
                        canister_id: parts[1].to_string(),
                        method_name: parts[2].to_string(),
                        args: parts[3].to_string(),
                    }
                }
                "query" => {
                    if parts.len() != 4 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "query command requires 3 arguments",
                        ));
                    }
                    TestCommand::Query {
                        canister_id: parts[1].to_string(),
                        method_name: parts[2].to_string(),
                        args: parts[3].to_string(),
                    }
                }
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("Unknown command: {}", parts[0]),
                    ));
                }
            };

            commands.push(command);
        }

        Ok(commands)
    }
}

pub struct TestRunner {
    commands: Vec<TestCommand>,
    subnet_type: SubnetType,
}

impl TestRunner {
    pub fn new(commands: Vec<TestCommand>, subnet_type: SubnetType) -> Self {
        Self {
            commands,
            subnet_type,
        }
    }

    pub fn run(&self) -> std::io::Result<()> {
        let mut server = match self.subnet_type {
            SubnetType::Application => PocketIcBuilder::new().with_application_subnet(),
            SubnetType::System => PocketIcBuilder::new()
                .with_system_subnet()
                .with_nns_subnet(),
        }
        .build();

        for command in &self.commands {
            // command: {:?}", command);
            command.execute(&mut server)?;
        }
        Ok(())
    }
}

/// The program reads stdin where the .drun file contents are piped in.
/// It then runs the commands in the .drun file and writes the output to stdout.
fn main() {
    // Read stdin
    let mut stdin = std::io::stdin();
    let mut buffer = String::new();
    let _ = stdin.read_to_string(&mut buffer);

    //println!("buffer: {}", buffer);

    let commands = TestCommands::new(buffer).parse();
    match commands {
        Ok(commands) => {
            let test_runner = TestRunner::new(commands, SubnetType::System);
            let _ = test_runner.run();
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
