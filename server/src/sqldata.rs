use crate::data::{
  Allocation, ListProject, PayEntry, PayType, Project, ProjectEdit, ProjectMember, ProjectTime,
  Role, SaveAllocation, SavePayEntry, SaveProject, SaveProjectEdit, SaveProjectInvoice,
  SaveProjectTime, SaveTimeEntry, SavedProject, SavedProjectEdit, TimeEntry, User, UserInviteData,
};
use crate::migrations as tm;
use barrel::backend::Sqlite;
use log::info;
use orgauth::data::RegistrationData;
use orgauth::endpoints::Callbacks;
use orgauth::util::now;
use rusqlite::{params, Connection};
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

pub fn timeclonk_callbacks() -> Callbacks {
  Callbacks {
    on_new_user: Box::new(on_new_user),
    extra_login_data: Box::new(extra_login_data_callback),
    on_delete_user: Box::new(on_delete_user),
  }
}

pub fn on_new_user(
  conn: &Connection,
  _rd: &RegistrationData,
  data: Option<String>,
  creator: Option<i64>,
  uid: i64,
) -> Result<(), orgauth::error::Error> {
  match data {
    Some(d) => {
      let invitedata: UserInviteData = serde_json::from_str(d.as_str())?;
      match creator {
        Some(cuid) => {
          for p in invitedata.projects {
            match member_role(conn, cuid, p.id)? {
              Some(Role::Admin) => {
                conn.execute(
                  "insert into projectmember (project, user, role)
                   values (?1, ?2, ?3)
                   on conflict (project, user) do update set role = ?3",
                  params![p.id, uid, p.role.to_string().as_str()],
                )?;
              }
              Some(_) => (),
              None => (),
            }
          }
        }
        None => (),
      }

      Ok(())
    }
    None => Ok(()),
  }
}

pub fn on_delete_user(_conn: &Connection, _uid: i64) -> Result<bool, orgauth::error::Error> {
  Ok(true)
}

// callback to pass to orgauth
pub fn extra_login_data_callback(
  _conn: &Connection,
  _uid: i64,
) -> Result<Option<serde_json::Value>, orgauth::error::Error> {
  Ok(None)
}

pub fn connection_open(dbfile: &Path) -> Result<Connection, orgauth::error::Error> {
  let conn = Connection::open(dbfile)?;

  // conn.busy_timeout(Duration::from_millis(500))?;
  conn.busy_handler(Some(|count| {
    info!("busy_handler: {}", count);
    let d = Duration::from_millis(500);
    std::thread::sleep(d);
    true
  }))?;

  conn.execute("PRAGMA foreign_keys = true;", params![])?;

  Ok(conn)
}

pub fn get_single_value(
  conn: &Connection,
  name: &str,
) -> Result<Option<String>, orgauth::error::Error> {
  match conn.query_row(
    "select value from singlevalue where name = ?1",
    params![name],
    |row| Ok(row.get(0)?),
  ) {
    Ok(v) => Ok(Some(v)),
    Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
    Err(x) => Err(x.into()),
  }
}

pub fn set_single_value(
  conn: &Connection,
  name: &str,
  value: &str,
) -> Result<(), orgauth::error::Error> {
  conn.execute(
    "insert into singlevalue (name, value) values (?1, ?2)
        on conflict (name) do update set value = ?2 where name = ?1",
    params![name, value],
  )?;
  Ok(())
}

pub fn dbinit(
  dbfile: &Path,
  token_expiration_ms: Option<i64>,
) -> Result<(), orgauth::error::Error> {
  let exists = dbfile.exists();

  let conn = connection_open(dbfile)?;

  if !exists {
    info!("initialdb");
    conn.execute_batch(tm::initialdb().make::<Sqlite>().as_str())?;
  }

  let nlevel = match get_single_value(&conn, "migration_level") {
    Err(_) => 0,
    Ok(None) => 0,
    Ok(Some(level)) => {
      let l = match level.parse::<i32>() {
        Ok(i) => i,
        Err(e) => return Err(format!("migration level parse error {:?}", e).into()),
      };
      l
    }
  };

  if nlevel < 1 {
    info!("udpate1");
    conn.execute_batch(tm::udpate1().make::<Sqlite>().as_str())?;
    set_single_value(&conn, "migration_level", "1")?;
  }
  if nlevel < 2 {
    info!("udpate2");
    tm::udpate2(&dbfile)?;
    set_single_value(&conn, "migration_level", "2")?;
  }
  if nlevel < 3 {
    info!("udpate3");
    conn.execute_batch(tm::udpate3().make::<Sqlite>().as_str())?;
    set_single_value(&conn, "migration_level", "3")?;
  }
  if nlevel < 4 {
    info!("udpate4");
    tm::udpate4(&dbfile)?;
    set_single_value(&conn, "migration_level", "4")?;
  }
  if nlevel < 5 {
    info!("udpate5");
    tm::udpate5(&dbfile)?;
    set_single_value(&conn, "migration_level", "5")?;
  }
  if nlevel < 6 {
    info!("udpate6");
    tm::udpate6(&dbfile)?;
    set_single_value(&conn, "migration_level", "6")?;
  }
  if nlevel < 7 {
    info!("udpate7");
    tm::udpate7(&dbfile)?;
    set_single_value(&conn, "migration_level", "7")?;
  }
  if nlevel < 8 {
    info!("udpate8");
    tm::udpate8(&dbfile)?;
    set_single_value(&conn, "migration_level", "8")?;
  }
  if nlevel < 9 {
    info!("udpate9");
    tm::udpate9(&dbfile)?;
    set_single_value(&conn, "migration_level", "9")?;
  }
  if nlevel < 10 {
    info!("udpate10");
    tm::udpate10(&dbfile)?;
    set_single_value(&conn, "migration_level", "10")?;
  }
  if nlevel < 11 {
    info!("udpate11");
    tm::udpate11(&dbfile)?;
    set_single_value(&conn, "migration_level", "11")?;
  }
  if nlevel < 12 {
    info!("udpate12");
    tm::udpate12(&dbfile)?;
    set_single_value(&conn, "migration_level", "12")?;
  }

  info!("db up to date.");

  if let Some(expms) = token_expiration_ms {
    orgauth::dbfun::purge_login_tokens(&conn, expms)?;
  }

  Ok(())
}

// --------------------------------------------------------

pub fn member_role(
  conn: &Connection,
  uid: i64,
  pid: i64,
) -> Result<Option<Role>, orgauth::error::Error> {
  match conn.query_row(
    "select role from projectmember where project = ?1 and user = ?2",
    params![pid, uid],
    |row| Ok(row.get::<usize, String>(0)?),
  ) {
    Ok(v) => match Role::from_str(v.as_str()) {
      Ok(r) => Ok(Some(r)),
      Err(_) => Ok(None),
    },
    Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
    Err(x) => Err(x.into()),
  }
}

pub fn project_list(
  conn: &Connection,
  uid: i64,
) -> Result<Vec<ListProject>, orgauth::error::Error> {
  // projects ordered by last clonk.
  let mut pstmt = conn.prepare(
    "select project.id, project.name, projectmember.role from project, projectmember,
      (select project, max(startdate) as sd from timeentry where user = ?1 group by project) te
    where
    project.id = projectmember.project and
    te.project = project.id and
    projectmember.user = ?1
    order by te.sd desc
    ",
  )?;
  let mut r: Vec<ListProject> = pstmt
    .query_map(params![uid], |row| {
      let role: String = row.get(2)?;
      Ok(ListProject {
        id: row.get(0)?,
        name: row.get(1)?,
        role: match Role::from_str(role.as_str()) {
          Ok(r) => r,
          Err(_) => Role::Observer, // default to observer on decode failure.  lame
        },
      })
    })?
    .filter_map(|x| x.ok())
    .collect();

  // projects without any clonks.
  let mut pstmt = conn.prepare(
    "select project.id, project.name, projectmember.role from project, projectmember
    where
    project.id = projectmember.project and
    projectmember.user = ?1 and
    not exists (select * from timeentry where project = project.id and user = ?1)
    ",
  )?;
  let mut rempty: Vec<ListProject> = pstmt
    .query_map(params![uid], |row| {
      let role: String = row.get(2)?;
      Ok(ListProject {
        id: row.get(0)?,
        name: row.get(1)?,
        role: match Role::from_str(role.as_str()) {
          Ok(r) => r,
          Err(_) => Role::Observer, // default to observer on decode failure.  lame
        },
      })
    })?
    .filter_map(|x| x.ok())
    .collect();

  // append unclonked projects onto the by-clonk results.
  r.append(&mut rempty);
  Ok(r)
}

pub fn save_project_edit(
  conn: &Connection,
  user: i64,
  project_edit: SaveProjectEdit,
) -> Result<SavedProjectEdit, orgauth::error::Error> {
  let sp = save_project(conn, user, project_edit.project)?;

  for m in project_edit.members {
    if m.delete {
      conn.execute(
        "delete from projectmember
         where user = ?1 and project = ?2",
        params![sp.id, m.id],
      )?;
    } else {
      conn.execute(
        "insert into projectmember (project, user, role)
         values (?1, ?2, ?3)
         on conflict (project, user) do update set role = ?3",
        params![sp.id, m.id, m.role.to_string().as_str()],
      )?;
    }
  }

  let proj = read_project(conn, sp.id)?;
  let mems = member_list(conn, sp.id)?;

  Ok(SavedProjectEdit {
    project: proj,
    members: mems,
  })
}

pub fn save_project_invoice(
  conn: &Connection,
  user: i64,
  project: SaveProjectInvoice,
) -> Result<Project, orgauth::error::Error> {
  let now = orgauth::util::now()?;
  conn.execute(
    "update project set invoice_seq = ?1,
                            extra_fields = ?2,
                            changeddate = ?3
          where id = ?4",
    params![
      project.invoice_seq,
      serde_json::to_value(project.extra_fields)?.to_string(),
      now,
      project.id
    ],
  )?;

  let proj = read_project(conn, project.id)?;

  Ok(proj)
}

pub fn save_project(
  conn: &Connection,
  user: i64,
  project: SaveProject,
) -> Result<SavedProject, orgauth::error::Error> {
  let now = now()?;

  let proj = match project.id {
    Some(id) => {
      conn.execute(
        "update project set name = ?1,
                            description = ?2,
                            due_days = ?3,
                            extra_fields = ?4,
                            invoice_id_template = ?5,
                            invoice_seq = ?6,
                            payer = ?7,
                            payee = ?8,
                            generic_task = ?9,
                            public = ?10,
                            rate = ?11,
                            currency = ?12,
                            changeddate = ?13
          where id = ?14",
        params![
          project.name,
          project.description,
          project.due_days,
          serde_json::to_value(project.extra_fields)?.to_string(),
          project.invoice_id_template,
          project.invoice_seq,
          project.payer,
          project.payee,
          project.generic_task,
          project.public,
          project.rate,
          project.currency,
          now,
          id
        ],
      )?;
      SavedProject {
        id: id,
        changeddate: now,
      }
    }
    None => {
      conn.execute(
        "insert into project (name, description, due_days, extra_fields, invoice_id_template, invoice_seq, payer, payee, generic_task, public, rate, currency, createdate, changeddate)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14)",
        params![
          project.name,
          project.description,
          project.due_days,
          serde_json::to_value(project.extra_fields)?.to_string(),
          project.invoice_id_template,
          project.invoice_seq,
          project.payer,
          project.payee,
          project.generic_task,
          project.public,
          project.rate,
          project.currency,
          now,
          now
        ],
      )?;
      let id = conn.last_insert_rowid();
      conn.execute(
        "insert into projectmember (project, user, role)
         values (?1, ?2, 'Admin')",
        params![id, user],
      )?;
      SavedProject {
        id: id,
        changeddate: now,
      }
    }
  };
  Ok(proj)
}

pub fn read_project(conn: &Connection, projectid: i64) -> Result<Project, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
    "select project.id,
            project.name,
            project.description,
            project.due_days,
            project.extra_fields,
            project.invoice_id_template,
            project.invoice_seq,
            project.payer,
            project.payee,
            project.generic_task,
            project.public,
            project.rate,
            project.currency,
            project.createdate,
            project.changeddate
      from project, projectmember where
      project.id = ?1",
  )?;
  let r = Ok(pstmt.query_row(params![projectid], |row| {
    Ok(Project {
      id: row.get(0)?,
      name: row.get(1)?,
      description: row.get(2)?,
      due_days: row.get(3)?,
      extra_fields: serde_json::from_str(
        row
          .get::<usize, Option<String>>(4)?
          .unwrap_or("{}".to_string())
          .as_str(),
      )
      .unwrap_or(Vec::new()),
      invoice_id_template: row.get(5)?,
      invoice_seq: row.get(6)?,
      payer: row.get(7)?,
      payee: row.get(8)?,
      generic_task: row.get(9)?,
      public: row.get(10)?,
      rate: row.get(11)?,
      currency: row.get(12)?,
      createdate: row.get(13)?,
      changeddate: row.get(14)?,
    })
  })?);
  r
}

pub fn member_list(
  conn: &Connection,
  projectid: i64,
) -> Result<Vec<ProjectMember>, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
        "select orgauth_user.id, orgauth_user.name, projectmember.role from orgauth_user, projectmember where
          orgauth_user.id = projectmember.user and
          projectmember.project = ?1",
      )?;
  let r = pstmt
    .query_map(params![projectid], |row| {
      match Role::from_str(row.get::<usize, String>(2)?.as_str()) {
        Ok(role) => Ok(ProjectMember {
          id: row.get(0)?,
          name: row.get(1)?,
          role: role,
        }),
        Err(_) => {
          // TODO this is a misuse of the rusqlite error.
          Err(rusqlite::Error::InvalidColumnType(
            2,
            "role".to_string(),
            rusqlite::types::Type::Text,
          ))
        }
      }
    })?
    .filter_map(|x| x.ok())
    .collect();

  // TODO: return error in case of error, instead of just leaving records out.

  Ok(r)
}

pub fn user_list(conn: &Connection) -> Result<Vec<User>, orgauth::error::Error> {
  let mut pstmt = conn.prepare("select orgauth_user.id, orgauth_user.name from orgauth_user")?;
  let r = pstmt
    .query_map(params![], |row| {
      Ok(User {
        id: row.get(0)?,
        name: row.get(1)?,
      })
    })?
    .filter_map(|x| x.ok())
    .collect();

  // TODO: return error in case of error, instead of just leaving records out.

  Ok(r)
}

pub fn read_project_edit(
  conn: &Connection,
  projectid: i64,
) -> Result<ProjectEdit, orgauth::error::Error> {
  let proj = read_project(conn, projectid)?;
  let members = member_list(conn, projectid)?;
  Ok(ProjectEdit {
    project: proj,
    members: members,
  })
}

pub fn user_time(conn: &Connection, userid: i64) -> Result<Vec<TimeEntry>, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
    "select te.id, te.project, te.user, te.description, te.startdate, te.enddate, te.ignore, te.createdate, te.changeddate, te.creator
          from timeentry te where
    te.user = ?1",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![userid], |row| {
        Ok(TimeEntry {
          id: row.get(0)?,
          project: row.get(1)?,
          user: row.get(2)?,
          description: row.get(3)?,
          startdate: row.get(4)?,
          enddate: row.get(5)?,
          ignore: row.get(6)?,
          createdate: row.get(7)?,
          changeddate: row.get(8)?,
          creator: row.get(9)?,
        })
      })?
      .filter_map(|x| x.ok())
      .collect(),
  );
  r
}

pub fn time_entries(
  conn: &Connection,
  projectid: i64,
) -> Result<Vec<TimeEntry>, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
    "select te.id, te.project, te.user, te.description, te.startdate, te.enddate, te.ignore, te.createdate, te.changeddate, te.creator
          from timeentry te where
    te.project = ?1",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid], |row| {
        Ok(TimeEntry {
          id: row.get(0)?,
          project: row.get(1)?,
          user: row.get(2)?,
          description: row.get(3)?,
          startdate: row.get(4)?,
          enddate: row.get(5)?,
          ignore: row.get(6)?,
          createdate: row.get(7)?,
          changeddate: row.get(8)?,
          creator: row.get(9)?,
        })
      })?
      .filter_map(|x| x.ok())
      .collect(),
  );
  r
}

pub fn pay_entries(
  conn: &Connection,
  projectid: i64,
) -> Result<Vec<PayEntry>, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
    "select  pe.id, pe.project, pe.user, pe.duration, pe.type, pe.paymentdate, pe.description, pe.createdate, pe.changeddate, pe.creator
          from payentry pe where
          pe.project = ?1",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid], |row| {
        Ok(PayEntry {
          id: row.get(0)?,
          project: row.get(1)?,
          user: row.get(2)?,
          duration: row.get(3)?,
          paytype: {
            let pt: i64 = row.get(4)?;
            if pt == 0 {
              PayType::Invoiced
            } else {
              PayType::Paid
            }
          },
          paymentdate: row.get(5)?,
          description: row.get(6)?,
          createdate: row.get(7)?,
          changeddate: row.get(8)?,
          creator: row.get(9)?,
        })
      })?
      .filter_map(|x| x.ok())
      .collect(),
  );
  r
}

pub fn save_pay_entry(
  conn: &Connection,
  uid: i64,
  spe: SavePayEntry,
) -> Result<i64, orgauth::error::Error> {
  let now = now()?;
  let pt = match spe.paytype {
    PayType::Invoiced => 0,
    PayType::Paid => 1,
  };
  match spe.id {
    Some(id) =>
        conn.execute(
          "update payentry set
              project =?1
            , user =?2
            , description =?3
            , duration =?4
            , type =?5
            , paymentdate =?6
            , changeddate =?7
              where id = ?8 ",
          params![spe.project, spe.user, spe.description, spe.duration,  pt, spe.paymentdate, now, id],
        )?,
    None =>
      conn.execute(
        "insert into payentry (project, user, description, duration, type, paymentdate, createdate, changeddate, creator)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
        params![spe.project, spe.user, spe.description, spe.duration, pt, spe.paymentdate, now, now, uid],
      )?,
  };
  let id = conn.last_insert_rowid();
  Ok(id)
}

// check for user membership before calling!
pub fn delete_pay_entry(
  conn: &Connection,
  _uid: i64,
  peid: i64,
) -> Result<(), orgauth::error::Error> {
  conn.execute("delete from payentry where id = ?1", params![peid])?;
  Ok(())
}

pub fn allocations(
  conn: &Connection,
  projectid: i64,
) -> Result<Vec<Allocation>, orgauth::error::Error> {
  let mut pstmt = conn.prepare(
    "select  a.id, a.project, a.duration, a.allocationdate, a.description, a.createdate, a.changeddate, a.creator
          from allocation a, projectmember pm where
          a.project = ?1",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid], |row| {
        Ok(Allocation {
          id: row.get(0)?,
          project: row.get(1)?,
          duration: row.get(2)?,
          allocationdate: row.get(3)?,
          description: row.get(4)?,
          createdate: row.get(5)?,
          changeddate: row.get(6)?,
          creator: row.get(7)?,
        })
      })?
      .filter_map(|x| x.ok())
      .collect(),
  );
  r
}

pub fn save_allocation(
  conn: &Connection,
  uid: i64,
  sa: SaveAllocation,
) -> Result<i64, orgauth::error::Error> {
  let now = now()?;
  match sa.id {
    Some(id) =>
        conn.execute(
          "update allocation set
              project =?1
            , description =?2
            , duration =?3
            , allocationdate =?4
            , changeddate =?5
              where id = ?6 ",
          params![sa.project, sa.description, sa.duration, sa.allocationdate, now, id],
        )?,
    None =>
      conn.execute(
        "insert into allocation (project, description, duration, allocationdate, createdate, changeddate, creator)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
        params![sa.project, sa.description, sa.duration, sa.allocationdate, now, now, uid],
      )?,
  };
  let id = conn.last_insert_rowid();
  Ok(id)
}

// check for user membership before calling!
pub fn delete_allocation(
  conn: &Connection,
  _uid: i64,
  id: i64,
) -> Result<(), orgauth::error::Error> {
  conn.execute("delete from allocation where id = ?1", params![id])?;
  Ok(())
}

pub fn is_project_member(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<bool, orgauth::error::Error> {
  match conn.query_row(
    "select user from projectmember where user = ?1 and project = ?2",
    params![uid, projectid],
    |_row| Ok(()),
  ) {
    Ok(_v) => Ok(true),
    Err(rusqlite::Error::QueryReturnedNoRows) => Ok(false),
    Err(x) => Err(x.into()),
  }
}

pub fn read_project_time(
  conn: &Connection,
  projectid: i64,
) -> Result<ProjectTime, orgauth::error::Error> {
  let proj = read_project(conn, projectid)?;
  let members = member_list(conn, projectid)?;
  let timeentries = time_entries(conn, projectid)?;
  let payentries = pay_entries(conn, projectid)?;
  let allocations = allocations(conn, projectid)?;
  Ok(ProjectTime {
    project: proj,
    members: members,
    timeentries: timeentries,
    payentries: payentries,
    allocations: allocations,
  })
}

pub fn save_time_entry(
  conn: &Connection,
  uid: i64,
  spt: SaveTimeEntry,
) -> Result<i64, orgauth::error::Error> {
  let now = now()?;
  match spt.id {
    Some(id) =>
      conn.execute(
        "update timeentry set
            project =?1,
            user = ?2,
            description = ?3,
            startdate = ?4,
            enddate = ?5,
            ignore = ?6,
            changeddate = ?7
          where id = ?8",
        params![spt.project, spt.user, spt.description, spt.startdate, spt.enddate, spt.ignore, now, id],
      )?,
    None =>
      conn.execute(
        "insert into timeentry (project, user, description, startdate, enddate, ignore, createdate, changeddate, creator)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
        params![spt.project, spt.user, spt.description, spt.startdate, spt.enddate, spt.ignore, now, now, uid],
      )?,
  };
  let id = conn.last_insert_rowid();
  Ok(id)
}

// check for user membership before calling!
pub fn delete_time_entry(
  conn: &Connection,
  _uid: i64,
  teid: i64,
) -> Result<(), orgauth::error::Error> {
  conn.execute("delete from timeentry where id = ?1", params![teid])?;
  Ok(())
}

pub fn save_project_time(
  conn: &Connection,
  uid: i64,
  spt: SaveProjectTime,
) -> Result<ProjectTime, orgauth::error::Error> {
  // is user a member of this project?
  if is_project_member(conn, uid, spt.project)? {
    for te in spt.savetimeentries {
      save_time_entry(conn, uid, te)?;
    }
    for id in spt.deletetimeentries {
      delete_time_entry(conn, uid, id)?;
    }
    for te in spt.savepayentries {
      save_pay_entry(conn, uid, te)?;
    }
    for id in spt.deletepayentries {
      delete_pay_entry(conn, uid, id)?;
    }
    for te in spt.saveallocations {
      save_allocation(conn, uid, te)?;
    }
    for id in spt.deleteallocations {
      delete_allocation(conn, uid, id)?;
    }
  }

  read_project_time(conn, spt.project)
}
