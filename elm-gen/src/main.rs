use std::path::Path;

use orgauth::util;
use protocol::{
  data::{
    Allocation, ExtraField, InvoiceItem, ListProject, PayEntry, PayType, PrintInvoice, Project,
    ProjectEdit, ProjectId, ProjectMember, ProjectTime, Role, SaveAllocation, SavePayEntry,
    SaveProject, SaveProjectEdit, SaveProjectInvoice, SaveProjectMember, SaveProjectTime,
    SaveTimeEntry, SavedProjectEdit, TimeEntry, User,
  },
  messages::{PublicMessageX, PublicResponseX, TcMessageX, TcResponseX, TimeClonkError},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let ed = Path::new("../elm/src");

  {
    let mut target = vec![];
    // elm_rs provides a macro for conveniently creating an Elm module with everything needed
    elm_rs::export!(
        "TcProtocol",
        &mut target,
        {        // generates types and encoders for types implementing ElmEncoder
        encoders: [
          Allocation,
          ExtraField,
          InvoiceItem,
          PayEntry,
          PayType,
          PrintInvoice,
          Project,
          ProjectId,
          ProjectMember,
          ProjectTime,
          PublicMessageX,
          Role,
          SaveAllocation,
          SavePayEntry,
          SaveProject,
          SaveProjectEdit,
          SaveProjectInvoice,
          SaveProjectMember,
          SaveProjectTime,
          SaveTimeEntry,
          SavedProjectEdit,
          TcMessageX,
          TimeEntry,
        ],
        // generates types and decoders for types implementing ElmDecoder
        decoders: [
          Allocation,
          ExtraField,
          ListProject,
          PayEntry,
          PayType,
          Project,
          ProjectEdit,
          ProjectId,
          ProjectMember,
          ProjectTime,
          PublicResponseX,
          Role,
          SavedProjectEdit,
          TcResponseX,
          TimeClonkError,
          TimeEntry,
          User,
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

    let outf = ed
      .join("TcProtocol.elm")
      .to_str()
      .expect("bad path")
      .to_string();
    util::write_string(outf.as_str(), uidout.as_str())?;
    println!("wrote file: {}", outf);
  }

  Ok(())
}
