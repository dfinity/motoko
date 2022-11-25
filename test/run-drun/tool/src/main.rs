use std::{fs, path::Path};

fn main() {
    let prefix = "core.";
    let directory = ".";
    let files: Vec<String> = fs::read_dir(directory)
        .expect("invalid directory")
        .map(|f| f.unwrap().path().display().to_string())
        .filter(|n| get_file_name(n).starts_with(&prefix))
        .collect();
    for file in files.iter() {
        dump(file);
    }
}

fn dump(file: &str) {
    println!("Core dump of {file}:");
    let content = fs::read(file).expect("invalid file");
    for (count, byte) in content.iter().enumerate() {
        print!("{byte:02x} ");
        if count % 32 == 0 {
            println!();
        }
    }
    println!();
    println!();
}

fn get_file_name(path: &str) -> &str {
    Path::new(path).file_name().unwrap().to_str().unwrap()
}
