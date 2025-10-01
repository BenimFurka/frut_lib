use frut_lib::{parse_files, analyze_project, File as FrutFile};

fn main() {
    let files = vec![FrutFile {
        path: "main.ft".to_string(),
        code: "var x: int = 42;".to_string(),
        ast: None
    }];
    
    let mut project = parse_files(files).project;
    
    if let Err(errors) = analyze_project(&mut project) {
        println!("Found {} errors", errors.len());
    } else {
        println!("Analysis successful");
    }
}