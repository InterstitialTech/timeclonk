use std::path::Path;

use orgauth::util;
use timeclonk_server::{
  data::{
    Allocation, ExtraField, ListProject, PayEntry, PayType, Project, ProjectEdit, ProjectMember,
    ProjectTime, Role, SaveAllocation, SavePayEntry, SaveProjectInvoice, SaveProjectTime,
    SaveTimeEntry, SavedProjectEdit, TimeEntry, User,
  },
  messages::{PublicMessageX, ServerResponseX, UserMessageX},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let ed = Path::new("../elm/src/");

  // --------------------------------------------------------------------------
  // Data.elm
  {
    let mut target = vec![];
    // elm_rs provides a macro for conveniently creating an Elm module with everything needed
    elm_rs::export!(
        "Data",
        &mut target,
        {
        encoders: [
          Allocation,
          ExtraField,
          PayEntry,
          PayType,
          Project,
          ProjectMember,
          PublicMessageX,
          Role,
          SaveAllocation,
          SavePayEntry,
          SaveProjectInvoice,
          SaveProjectTime,
          SaveTimeEntry,
          SavedProjectEdit,
          UserMessageX
        ],
        decoders: [
          Allocation,
          ExtraField,
          ListProject,
          PayEntry,
          PayType,
          Project,
          ProjectEdit,
          ProjectMember,
          ProjectTime,
          Role,
          SavedProjectEdit,
          ServerResponseX,
          TimeEntry,
          User
        ],
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

    let outf = ed.join("DataX.elm").to_str().expect("bad path").to_string();
    println!("writing file: {}", outf);
    util::write_string(outf.as_str(), uidout.as_str())?;
    println!("wrote file: {}", outf);
  }

  Ok(())
}
