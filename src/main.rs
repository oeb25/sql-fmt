extern crate sql_fmt;

use sql_fmt::prettify;

use std::os::raw::c_char;
use std::ffi::CStr;
use std::ffi::CString;

fn my_string_safe(i: *mut c_char) -> String {
  unsafe {
    CStr::from_ptr(i).to_string_lossy().into_owned()
  }
}

#[no_mangle]
pub extern "C" fn wasm_prettify(s: *mut c_char) -> *mut c_char {
  let data = my_string_safe(s);

  let f = prettify(&data, None).unwrap();

  CString::new(f.as_str())
    .unwrap()
    .into_raw()
}

fn main() {
    use std::io::Read;
    std::env::args().skip(1).next().map(|name| {
        let mut d = String::new();
        std::fs::File::open(name).map(|mut file| {
            file.read_to_string(&mut d).unwrap();

            let a = prettify(&d, None).unwrap();
            println!("{}", a);

            debug_assert_eq!(a, prettify(&a, None).unwrap());
        });
    });
}
