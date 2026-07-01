use std::path::Path;

use orgauth::util;
use protocol::messages::{PublicMessageX, ServerResponseX, UserMessageX};

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let ed = Path::new("../elm/src");

  // --------------------------------------------------------------------------
  // Data.elm
  {
    let mut target = vec![];
    // elm_rs provides a macro for conveniently creating an Elm module with everything needed
    elm_rs::export!(
        "Data",
        &mut target,
        {        // generates types and encoders for types implementing ElmEncoder
        encoders: [ServerResponseX],
        // generates types and decoders for types implementing ElmDecoder
        decoders: [PublicMessageX, UserMessageX],
        // generates types and functions for forming queries for types implementing ElmQuery
        queries: [],
        // generates types and functions for forming queries for types implementing ElmQueryField
        query_fields: [],
        }
    )
    .unwrap();

    let output = String::from_utf8(target).unwrap();

    // add line importing Orgauth.Userid
    let uidout = output.replace(
      "import Json.Encode",
      r#"import Json.Encode
import Orgauth.Data exposing (UserId(..), userIdDecoder, userIdEncoder)"#,
    );

    let outf = ed
      .join("NwData.elm")
      .to_str()
      .expect("bad path")
      .to_string();
    util::write_string(outf.as_str(), uidout.as_str())?;
    println!("wrote file: {}", outf);
  }

  Ok(())
}
