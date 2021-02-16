use std::process::Command;

fn main() {
    let desc = match Command::new("git")
        .args(&["describe", "--tags", "--long"])
        .output()
    {
        Ok(output) => String::from_utf8_lossy(&output.stdout).to_string(),
        Err(_) => "unknown".into(),
    };
    println!("cargo:rustc-env=NOTEBACKEND_GIT_DESC={}", desc);
}
