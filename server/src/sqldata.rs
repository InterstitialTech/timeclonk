use crate::data::{
  Allocation, ListProject, PayEntry, Project, ProjectEdit, ProjectMember, ProjectTime,
  SaveAllocation, SavePayEntry, SaveProject, SaveProjectEdit, SaveProjectTime, SaveTimeEntry,
  SavedProject, SavedProjectEdit, TimeEntry,
};
use crate::util::{is_token_expired, now};
use barrel::backend::Sqlite;
use barrel::{types, Migration};
use crypto_hash::{hex_digest, Algorithm};
use log::info;
use orgauth::data::{ChangeEmail, ChangePassword, LoginData, User};
use orgauth::migrations;
use rusqlite::{params, Connection};
use simple_error::bail;
use std::error::Error;
use std::path::Path;
use std::time::Duration;
use uuid::Uuid;

pub fn login_data(conn: &Connection, uid: i64) -> Result<LoginData, Box<dyn Error>> {
  let user = orgauth::dbfun::read_user_by_id(&conn, uid)?;
  Ok(LoginData {
    userid: uid,
    name: user.name,
    data: None,
  })
}

pub fn on_new_user(
  conn: &Connection,
  rd: &orgauth::data::RegistrationData,
  uid: i64,
) -> Result<(), Box<dyn Error>> {
  Ok(())
}

// callback to pass to orgauth
pub fn extra_login_data_callback(
  conn: &Connection,
  uid: i64,
) -> Result<Option<serde_json::Value>, Box<dyn Error>> {
  Ok(None)
}

pub fn connection_open(dbfile: &Path) -> Result<Connection, Box<dyn Error>> {
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

pub fn initialdb() -> Migration {
  let mut m = Migration::new();

  // migration basics, user stuff.

  // table for storing single values.  in particular migration numbers.
  m.create_table("singlevalue", |t| {
    t.add_column("name", types::text().nullable(false).unique(true));
    t.add_column("value", types::text().nullable(false));
  });

  m.create_table("user", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("name", types::text().nullable(false).unique(true));
    t.add_column("hashwd", types::text().nullable(false));
    t.add_column("salt", types::text().nullable(false));
    t.add_column("email", types::text().nullable(false));
    t.add_column("registration_key", types::text().nullable(true));
    t.add_column("createdate", types::integer().nullable(false));
  });

  // add token table.  multiple tokens per user to support multiple browsers and/or devices.
  m.create_table("token", |t| {
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("token", types::text().nullable(false));
    t.add_column("tokendate", types::integer().nullable(false));
    t.add_index("tokenunq", types::index(vec!["user", "token"]).unique(true));
  });

  // add newemail table.  each request for a new email creates an entry.
  m.create_table("newemail", |t| {
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("email", types::text().nullable(false));
    t.add_column("token", types::text().nullable(false));
    t.add_column("tokendate", types::integer().nullable(false));
    t.add_index(
      "newemailunq",
      types::index(vec!["user", "token"]).unique(true),
    );
  });

  // add newpassword table.  each request for a new password creates an entry.
  m.create_table("newpassword", |t| {
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("token", types::text().nullable(false));
    t.add_column("tokendate", types::integer().nullable(false));
    t.add_index(
      "resetpasswordunq",
      types::index(vec!["user", "token"]).unique(true),
    );
  });

  m
}

pub fn udpate1() -> Migration {
  let mut m = Migration::new();

  // timeclonk specific tables.

  m.create_table("project", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("name", types::text().nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("public", types::boolean().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
  });

  m.create_table("projectmember", |t| {
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_index("unq", types::index(vec!["project", "user"]).unique(true));
  });

  m.create_table("timeentry", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
    t.add_index(
      "timeentryunq",
      types::index(vec!["user", "startdate"]).unique(true),
    );
  });

  m.create_table("payentry", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("paymentdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
    t.add_index(
      "payentryunq",
      types::index(vec!["user", "paymentdate"]).unique(true),
    );
  });

  m
}

pub fn udpate2(dbfile: &Path) -> Result<(), Box<dyn Error>> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // temp table to hold zknote data.
  m1.create_table("timeentrytemp", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
    // t.add_index(
    //   "timeentryunq",
    //   types::index(vec!["user", "startdate"]).unique(true),
    // );
  });

  conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into timeentrytemp (
      id,
      project,
      user,
      description,
      startdate,
      enddate,
      createdate,
      changeddate,
      creator)
        select
      id,
      project,
      user,
      description,
      startdate,
      enddate,
      createdate,
      changeddate,
      creator from timeentry",
    params![],
  )?;

  let mut m2 = Migration::new();
  // drop zknote.
  m2.drop_table("timeentry");

  // add 'ignore' bool to timeentry.
  m2.create_table("timeentry", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("user", types::foreign("user", "id").nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("ignore", types::boolean().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
    t.add_index(
      "timeentryunq",
      types::index(vec!["user", "startdate"]).unique(true),
    );
  });

  conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // copy everything from the temp table.
  conn.execute(
    "insert into timeentry (
          id,
          project,
          user,
          description,
          startdate,
          enddate,
          ignore,
          createdate,
          changeddate,
          creator)
        select
          id,
          project,
          user,
          description,
          startdate,
          enddate,
          0,
          createdate,
          changeddate,
          creator
        from timeentrytemp",
    params![],
  )?;

  let mut m3 = Migration::new();
  // drop timeentrytemp.
  m3.drop_table("timeentrytemp");

  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  Ok(())
}

pub fn udpate3() -> Migration {
  let mut m = Migration::new();

  m.create_table("allocation", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("project", types::foreign("project", "id").nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("allocationdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
    t.add_index(
      "allocationunq",
      types::index(vec!["user", "allocationdate"]).unique(true),
    );
  });

  m
}

pub fn udpate4(dbfile: &Path) -> Result<(), Box<dyn Error>> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // temp table to hold data while we make a new table.
  m1.create_table("projecttemp", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("name", types::text().nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("public", types::boolean().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
  });

  conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into projecttemp (
      id,
      name,
      description,
      public,
      createdate,
      changeddate)
     select
      id,
      name,
      description,
      public,
      createdate,
      changeddate from project",
    params![],
  )?;

  let mut m2 = Migration::new();
  // drop zknote.
  m2.drop_table("project");

  // add 'rate' to timeentry.
  m2.create_table("project", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column("name", types::text().nullable(false));
    t.add_column("description", types::text().nullable(false));
    t.add_column("public", types::boolean().nullable(false));
    t.add_column("rate", types::float().nullable(true));
    t.add_column("currency", types::text().nullable(true));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
  });

  conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // copy everything from the temp table.
  conn.execute(
    "insert into project (
      id,
      name,
      description,
      public,
      createdate,
      changeddate)
     select
      id,
      name,
      description,
      public,
      createdate,
      changeddate from projecttemp",
    params![],
  )?;

  let mut m3 = Migration::new();
  // drop timeentrytemp.
  m3.drop_table("projecttemp");

  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  Ok(())
}

// --------------------------------------------------------------------------------
// orgauth enters the chat
// --------------------------------------------------------------------------------
pub fn udpate5(dbfile: &Path) -> Result<(), Box<dyn Error>> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;

  migrations::udpate1(dbfile)?;

  // copy from old user tables into orgauth tables.
  conn.execute(
    "insert into orgauth_user (id, name, hashwd, salt, email, registration_key, createdate)
        select id, name, hashwd, salt, email, registration_key, createdate from user",
    params![],
  )?;

  conn.execute(
    "insert into orgauth_token (user, token, tokendate)
        select user, token, tokendate from token",
    params![],
  )?;

  conn.execute(
    "insert into orgauth_newemail (user, email, token, tokendate)
        select user, email, token, tokendate from newemail",
    params![],
  )?;

  conn.execute(
    "insert into orgauth_newpassword (user, token, tokendate)
        select user, token, tokendate from newpassword",
    params![],
  )?;

  // // Hmmm would switch to 'zkuser', but I'll keep the user table so I don't have to rebuild every table with
  // // new foreign keys.
  // let mut m1 = Migration::new();

  // m1.create_table("usertemp", |t| {
  //   t.add_column("id", types::foreign("orgauth_user", "id").nullable(false));
  //   t.add_column("zknote", types::foreign("zknote", "id").nullable(true));
  //   t.add_column("homenote", types::foreign("zknote", "id").nullable(true));
  // });

  // conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // // copy from user.
  // conn.execute(
  //   "insert into usertemp (id, zknote, homenote)
  //       select id, zknote, homenote from user",
  //   params![],
  // )?;

  // let mut m2 = Migration::new();
  // m2.drop_table("user");

  // // new user table with homenote
  // m2.create_table("user", |t| {
  //   t.add_column("id", types::foreign("orgauth_user", "id").nullable(false));
  //   t.add_column("zknote", types::foreign("zknote", "id").nullable(true));
  //   t.add_column("homenote", types::foreign("zknote", "id").nullable(true));
  // });

  // conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // // copy everything from usertemp.
  // conn.execute(
  //   "insert into user (id, zknote, homenote)
  //       select id, zknote, homenote from usertemp",
  //   params![],
  // )?;

  let mut m3 = Migration::new();
  m3.drop_table("user");
  m3.drop_table("token");
  m3.drop_table("newemail");
  m3.drop_table("newpassword");

  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  // --------------------------------------------------------------------------------------
  // remake zknotes table to point at orgauth_user instead of user.
  // CREATE TABLE IF NOT EXISTS "singlevalue" ("name" TEXT NOT NULL UNIQUE, "value" TEXT NOT NULL);

  // CREATE TABLE IF NOT EXISTS "user" ("id" INTEGER PRIMARY KEY NOT NULL, "name" TEXT NOT NULL UNIQUE, "hashwd" TEXT NOT NULL, "salt" TEXT NOT NULL, "email" TEXT NOT NULL, "registration_key" TEXT, "createdate" INTEGER NOT NULL);
  // CREATE TABLE IF NOT EXISTS "token" ("user" INTEGER REFERENCES user(id) NOT NULL, "token" TEXT NOT NULL, "tokendate" INTEGER NOT NULL);
  // CREATE UNIQUE INDEX "tokenunq" ON "token" ("user", "token");
  // CREATE TABLE IF NOT EXISTS "newemail" ("user" INTEGER REFERENCES user(id) NOT NULL, "email" TEXT NOT NULL, "token" TEXT NOT NULL, "tokendate" INTEGER NOT NULL);
  // CREATE UNIQUE INDEX "newemailunq" ON "newemail" ("user", "token");
  // CREATE TABLE IF NOT EXISTS "newpassword" ("user" INTEGER REFERENCES user(id) NOT NULL, "token" TEXT NOT NULL, "tokendate" INTEGER NOT NULL);
  // CREATE UNIQUE INDEX "resetpasswordunq" ON "newpassword" ("user", "token");

  // create temp tables.
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"tempprojectmember\" (\"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL);
  CREATE TABLE IF NOT EXISTS \"temppayentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"paymentdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);
  CREATE TABLE IF NOT EXISTS \"temptimeentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL, \"description\" TEXT NOT NULL, \"startdate\" INTEGER NOT NULL, \"enddate\" INTEGER NOT NULL, \"ignore\" BOOLEAN NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);
  CREATE TABLE IF NOT EXISTS \"tempallocation\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"allocationdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);
  CREATE TABLE IF NOT EXISTS \"tempproject\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"name\" TEXT NOT NULL, \"description\" TEXT NOT NULL, \"public\" BOOLEAN NOT NULL, \"rate\" REAL, \"currency\" TEXT, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL);"
    ,params![],
  )?;

  // insert into temp tables.
  conn.execute(
   "insert into tempprojectmember (project, user)
     select project, user from projectmember;
   insert into temppayentry (id, project, user, description, duration, paymentdate, createdate, changeddate, creator)
     select id, project, user, description, duration, paymentdate, createdate, changeddate, creator from payentry;
   insert into temptimeentry (id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator)
     select id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator from timeentry;
   insert into tempallocation (id, project, description, duration, allocationdate, createdate, changeddate, creator)
     select id, project, description, duration, allocationdate, createdate, changeddate, creator from allocation;
   insert into tempproject (id, name, description, public, rate, currency, createdate, changeddate)
     select id, name, description, public, rate, currency, createdate, changeddate from project;"
    ,params![],
  )?;

  // drop tables.
  conn.execute(
    "drop table projectmember;
   drop table payentry;
   drop table timeentry;
   drop table allocation;
   drop table project;",
    params![],
  )?;

  // create tables that use orgauth_user foreighn keys and not user.
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"projectmember\" (\"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL);
  CREATE UNIQUE INDEX \"unq\" ON \"projectmember\" (\"project\", \"user\");
  CREATE TABLE IF NOT EXISTS \"payentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"paymentdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);
  CREATE UNIQUE INDEX \"payentryunq\" ON \"payentry\" (\"user\", \"paymentdate\");
  CREATE TABLE IF NOT EXISTS \"timeentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL, \"description\" TEXT NOT NULL, \"startdate\" INTEGER NOT NULL, \"enddate\" INTEGER NOT NULL, \"ignore\" BOOLEAN NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);
  CREATE UNIQUE INDEX \"timeentryunq\" ON \"timeentry\" (\"user\", \"startdate\");
  CREATE TABLE IF NOT EXISTS \"allocation\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"allocationdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);
  CREATE UNIQUE INDEX \"allocationunq\" ON \"allocation\" (\"user\", \"allocationdate\");
  CREATE TABLE IF NOT EXISTS \"project\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"name\" TEXT NOT NULL, \"description\" TEXT NOT NULL, \"public\" BOOLEAN NOT NULL, \"rate\" REAL, \"currency\" TEXT, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL);"
    ,params![],
  )?;

  // copy out of temp tables.
  conn.execute(
   "insert into projectmember (project, user)
     select project, user from tempprojectmember;
   insert into payentry (id, project, user, description, duration, paymentdate, createdate, changeddate, creator)
     select id, project, user, description, duration, paymentdate, createdate, changeddate, creator from temppayentry;
   insert into timeentry (id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator)
     select id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator from temptimeentry;
   insert into allocation (id, project, description, duration, allocationdate, createdate, changeddate, creator)
     select id, project, description, duration, allocationdate, createdate, changeddate, creator from tempallocation;
   insert into project (id, name, description, public, rate, currency, createdate, changeddate)
     select id, name, description, public, rate, currency, createdate, changeddate from tempproject;"
    ,params![],
  )?;

  // drop temp tables.
  conn.execute(
    "drop table tempprojectmember;
   drop table temppayentry;
   drop table temptimeentry;
   drop table tempallocation;
   drop table tempproject;",
    params![],
  )?;

  Ok(())
}

pub fn get_single_value(conn: &Connection, name: &str) -> Result<Option<String>, Box<dyn Error>> {
  match conn.query_row(
    "select value from singlevalue where name = ?1",
    params![name],
    |row| Ok(row.get(0)?),
  ) {
    Ok(v) => Ok(Some(v)),
    Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
    Err(x) => Err(Box::new(x)),
  }
}

pub fn set_single_value(conn: &Connection, name: &str, value: &str) -> Result<(), Box<dyn Error>> {
  conn.execute(
    "insert into singlevalue (name, value) values (?1, ?2)
        on conflict (name) do update set value = ?2 where name = ?1",
    params![name, value],
  )?;
  Ok(())
}

pub fn dbinit(dbfile: &Path, token_expiration_ms: i64) -> Result<(), Box<dyn Error>> {
  let exists = dbfile.exists();

  let conn = connection_open(dbfile)?;

  if !exists {
    info!("initialdb");
    conn.execute_batch(initialdb().make::<Sqlite>().as_str())?;
  }

  let nlevel = match get_single_value(&conn, "migration_level") {
    Err(_) => 0,
    Ok(None) => 0,
    Ok(Some(level)) => {
      let l = level.parse::<i32>()?;
      l
    }
  };

  if nlevel < 1 {
    info!("udpate1");
    conn.execute_batch(udpate1().make::<Sqlite>().as_str())?;
    set_single_value(&conn, "migration_level", "1")?;
  }
  if nlevel < 2 {
    info!("udpate2");
    udpate2(&dbfile)?;
    set_single_value(&conn, "migration_level", "2")?;
  }
  if nlevel < 3 {
    info!("udpate3");
    conn.execute_batch(udpate3().make::<Sqlite>().as_str())?;
    set_single_value(&conn, "migration_level", "3")?;
  }
  if nlevel < 4 {
    info!("udpate4");
    udpate4(&dbfile)?;
    set_single_value(&conn, "migration_level", "4")?;
  }
  if nlevel < 5 {
    info!("udpate5");
    udpate5(&dbfile)?;
    set_single_value(&conn, "migration_level", "5")?;
  }

  info!("db up to date.");

  orgauth::dbfun::purge_login_tokens(&conn, token_expiration_ms)?;

  Ok(())
}

// --------------------------------------------------------

pub fn project_list(conn: &Connection, uid: i64) -> Result<Vec<ListProject>, Box<dyn Error>> {
  let mut pstmt = conn.prepare(
    "select project.id, project.name from project, projectmember where
    project.id = projectmember.project and
    projectmember.user = ?1",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![uid], |row| {
        Ok(ListProject {
          id: row.get(0)?,
          name: row.get(1)?,
        })
      })?
      .filter_map(|x| x.ok())
      .collect(),
  );
  r
}

// password reset request.
pub fn save_project_edit(
  conn: &Connection,
  user: i64,
  project_edit: SaveProjectEdit,
) -> Result<SavedProjectEdit, Box<dyn Error>> {
  let sp = save_project(conn, user, project_edit.project)?;

  for m in project_edit.members {
    if m.delete {
      conn.execute(
        "insert into projectmember (project, user)
         values (?1, ?2)",
        params![sp.id, m.id],
      )?;
    } else {
      conn.execute(
        "insert into projectmember (project, user)
         values (?1, ?2)",
        params![sp.id, m.id],
      )?;
    }
  }

  let proj = read_project(conn, user, sp.id)?;
  let mems = member_list(conn, user, Some(sp.id))?;

  Ok(SavedProjectEdit {
    project: proj,
    members: mems,
  })
}

// password reset request.
pub fn save_project(
  conn: &Connection,
  user: i64,
  project: SaveProject,
) -> Result<SavedProject, Box<dyn Error>> {
  let now = now()?;

  let proj = match project.id {
    Some(id) => {
      conn.execute(
        "update project set name = ?1, description = ?2, public = ?3, rate = ?4, currency = ?5, changeddate = ?6
          where id = ?7",
        params![project.name, project.description, project.public, project.rate, project.currency, now, id],
      )?;
      SavedProject {
        id: id,
        changeddate: now,
      }
    }
    None => {
      conn.execute(
        "insert into project (name, description, public, rate, currency, createdate, changeddate)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
        params![
          project.name,
          project.description,
          project.public,
          project.rate,
          project.currency,
          now,
          now
        ],
      )?;
      let id = conn.last_insert_rowid();
      conn.execute(
        "insert into projectmember (project, user)
         values (?1, ?2)",
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

pub fn read_project(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<Project, Box<dyn Error>> {
  let mut pstmt = conn.prepare(
    "select project.id, project.name, project.description, project.public, project.rate, project.currency, project.createdate, project.changeddate
      from project, projectmember where
      project.id = projectmember.project and
      projectmember.user = ?1 and
      project.id = ?2",
  )?;
  let r = Ok(pstmt.query_row(params![uid, projectid], |row| {
    Ok(Project {
      id: row.get(0)?,
      name: row.get(1)?,
      description: row.get(2)?,
      public: row.get(3)?,
      rate: row.get(4)?,
      currency: row.get(5)?,
      createdate: row.get(6)?,
      changeddate: row.get(7)?,
    })
  })?);
  r
}

pub fn member_list(
  conn: &Connection,
  _uid: i64,
  projectid: Option<i64>,
) -> Result<Vec<ProjectMember>, Box<dyn Error>> {
  let r = match projectid {
    Some(projectid) => {
      let mut pstmt = conn.prepare(
        "select user.id, user.name from user, projectmember where
          user.id = projectmember.user and
          projectmember.project = ?1",
      )?;
      let r = pstmt
        .query_map(params![projectid], |row| {
          Ok(ProjectMember {
            id: row.get(0)?,
            name: row.get(1)?,
          })
        })?
        .filter_map(|x| x.ok())
        .collect();
      Ok(r)
    }
    None => {
      let mut pstmt = conn.prepare("select user.id, user.name from user")?;
      let r = pstmt
        .query_map(params![], |row| {
          Ok(ProjectMember {
            id: row.get(0)?,
            name: row.get(1)?,
          })
        })?
        .filter_map(|x| x.ok())
        .collect();
      Ok(r)
    }
  };
  r
}

pub fn read_project_edit(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<ProjectEdit, Box<dyn Error>> {
  let proj = read_project(conn, uid, projectid)?;
  let members = member_list(conn, uid, Some(projectid))?;
  Ok(ProjectEdit {
    project: proj,
    members: members,
  })
}

pub fn time_entries(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<Vec<TimeEntry>, Box<dyn Error>> {
  let mut pstmt = conn.prepare(
    "select te.id, te.project, te.user, te.description, te.startdate, te.enddate, te.ignore, te.createdate, te.changeddate, te.creator
          from timeentry te, projectmember pm where
    te.project = ?1 and
    te.project = pm.project and
    pm.user = ?2",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid, uid], |row| {
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
  uid: i64,
  projectid: i64,
) -> Result<Vec<PayEntry>, Box<dyn Error>> {
  let mut pstmt = conn.prepare(
    "select  pe.id, pe.project, pe.user, pe.duration, pe.paymentdate, pe.description, pe.createdate, pe.changeddate, pe.creator
          from payentry pe, projectmember pm where
          pe.project = ?1 and
          pe.project = pm.project and
          pm.user = ?2",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid, uid], |row| {
        Ok(PayEntry {
          id: row.get(0)?,
          project: row.get(1)?,
          user: row.get(2)?,
          duration: row.get(3)?,
          paymentdate: row.get(4)?,
          description: row.get(5)?,
          createdate: row.get(6)?,
          changeddate: row.get(7)?,
          creator: row.get(8)?,
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
) -> Result<i64, Box<dyn Error>> {
  let now = now()?;
  match spe.id {
    Some(id) =>
        conn.execute(
          "update payentry set
              project =?1
            , user =?2
            , description =?3
            , duration =?4
            , paymentdate =?5
            , changeddate =?6
              where id = ?7 ",
          params![spe.project, spe.user, spe.description, spe.duration, spe.paymentdate, now, id],
        )?,
    None =>
      conn.execute(
        "insert into payentry (project, user, description, duration, paymentdate, createdate, changeddate, creator)
         values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
        params![spe.project, spe.user, spe.description, spe.duration, spe.paymentdate, now, now, uid],
      )?,
  };
  let id = conn.last_insert_rowid();
  Ok(id)
}

// check for user membership before calling!
pub fn delete_pay_entry(conn: &Connection, _uid: i64, peid: i64) -> Result<(), Box<dyn Error>> {
  conn.execute("delete from payentry where id = ?1", params![peid])?;
  Ok(())
}

pub fn allocations(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<Vec<Allocation>, Box<dyn Error>> {
  let mut pstmt = conn.prepare(
    "select  a.id, a.project, a.duration, a.allocationdate, a.description, a.createdate, a.changeddate, a.creator
          from allocation a, projectmember pm where
          a.project = ?1 and
          a.project = pm.project and
          pm.user = ?2",
  )?;
  let r = Ok(
    pstmt
      .query_map(params![projectid, uid], |row| {
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
) -> Result<i64, Box<dyn Error>> {
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
pub fn delete_allocation(conn: &Connection, _uid: i64, id: i64) -> Result<(), Box<dyn Error>> {
  conn.execute("delete from allocation where id = ?1", params![id])?;
  Ok(())
}

pub fn is_project_member(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<bool, Box<dyn Error>> {
  match conn.query_row(
    "select user from projectmember where user = ?1 and project = ?2",
    params![uid, projectid],
    |_row| Ok(()),
  ) {
    Ok(_v) => Ok(true),
    Err(rusqlite::Error::QueryReturnedNoRows) => Ok(false),
    Err(x) => Err(Box::new(x)),
  }
}

pub fn read_project_time(
  conn: &Connection,
  uid: i64,
  projectid: i64,
) -> Result<ProjectTime, Box<dyn Error>> {
  let proj = read_project(conn, uid, projectid)?;
  let members = member_list(conn, uid, Some(projectid))?;
  let timeentries = time_entries(conn, uid, projectid)?;
  let payentries = pay_entries(conn, uid, projectid)?;
  let allocations = allocations(conn, uid, projectid)?;
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
) -> Result<i64, Box<dyn Error>> {
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
pub fn delete_time_entry(conn: &Connection, _uid: i64, teid: i64) -> Result<(), Box<dyn Error>> {
  conn.execute("delete from timeentry where id = ?1", params![teid])?;
  Ok(())
}

pub fn save_project_time(
  conn: &Connection,
  uid: i64,
  spt: SaveProjectTime,
) -> Result<ProjectTime, Box<dyn Error>> {
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

  read_project_time(conn, uid, spt.project)
}
