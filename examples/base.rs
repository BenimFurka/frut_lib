use frut_lib::parse_code;

fn main() {
    let result = parse_code("var x: int = 42;", "main.ft".to_string());
    match result.is_success() {
        false => println!("Found {} errors", result.errors.len()),
        true => println!("Analysis successful"),
    }
}