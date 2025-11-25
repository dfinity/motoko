wit_bindgen::generate!({
    path: "zstd.wit",
    world: "zstd",
});

use crate::exports::api::Guest;
use std::io::Cursor;

struct Zstd;
export!(Zstd);

impl Guest for Zstd {
    fn encode_all(data: Vec<u8>, level: i32) -> Result<Vec<u8>, String> {
        zstd::stream::encode_all(Cursor::new(data), level).map_err(|e| format!("compression failed: {:?}", e))
    }

    fn decode_all(data: Vec<u8>) -> Result<Vec<u8>, String> {
        zstd::stream::decode_all(Cursor::new(data)).map_err(|e| format!("decompression failed: {:?}", e))
    }
}
