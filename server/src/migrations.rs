use barrel::backend::Sqlite;
use barrel::{types, Migration};
use orgauth::migrations as om;
use rusqlite::{params, Connection};
use std::path::Path;

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
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("token", types::text().nullable(false));
    t.add_column("tokendate", types::integer().nullable(false));
    t.add_index("tokenunq", types::index(vec!["user", "token"]).unique(true));
  });

  // add newemail table.  each request for a new email creates an entry.
  m.create_table("newemail", |t| {
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("paymentdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_index(
      "payentryunq",
      types::index(vec!["user", "paymentdate"]).unique(true),
    );
  });

  m
}

pub fn udpate2(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // temp table to hold data while we make the new table.
  m1.create_table("timeentrytemp", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("startdate", types::integer().nullable(false));
    t.add_column("enddate", types::integer().nullable(false));
    t.add_column("ignore", types::boolean().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
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
  // drop temp table.
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
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("allocationdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_index(
      "allocationunq",
      types::index(vec!["user", "allocationdate"]).unique(true),
    );
  });

  m
}

pub fn udpate4(dbfile: &Path) -> Result<(), orgauth::error::Error> {
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
pub fn udpate5(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;

  om::udpate1(dbfile)?;

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

  // --------------------------------------------------------------------------------------
  // remake tables to point at orgauth_user instead of user.

  // create temp tables.
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"tempprojectmember\" (\"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"temppayentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"paymentdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"temptimeentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES user(id) NOT NULL, \"description\" TEXT NOT NULL, \"startdate\" INTEGER NOT NULL, \"enddate\" INTEGER NOT NULL, \"ignore\" BOOLEAN NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"tempallocation\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"allocationdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
  "CREATE TABLE IF NOT EXISTS \"tempproject\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"name\" TEXT NOT NULL, \"description\" TEXT NOT NULL, \"public\" BOOLEAN NOT NULL, \"rate\" REAL, \"currency\" TEXT, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL);"
    ,params![],
  )?;

  // insert into temp tables.
  conn.execute(
    "insert into tempprojectmember (project, user)
     select project, user from projectmember;",
    params![],
  )?;
  conn.execute(
   "insert into temppayentry (id, project, user, description, duration, paymentdate, createdate, changeddate, creator)
     select id, project, user, description, duration, paymentdate, createdate, changeddate, creator from payentry;"
    ,params![],
  )?;
  conn.execute(
   "insert into temptimeentry (id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator)
     select id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator from timeentry;"
    ,params![],
  )?;
  conn.execute(
   "insert into tempallocation (id, project, description, duration, allocationdate, createdate, changeddate, creator)
     select id, project, description, duration, allocationdate, createdate, changeddate, creator from allocation;"
    ,params![],
  )?;
  conn.execute(
   "insert into tempproject (id, name, description, public, rate, currency, createdate, changeddate)
     select id, name, description, public, rate, currency, createdate, changeddate from project;"
    ,params![],
  )?;

  // drop tables.
  conn.execute("drop table projectmember;", params![])?;
  conn.execute("drop table payentry;", params![])?;
  conn.execute("drop table timeentry;", params![])?;
  conn.execute("drop table allocation;", params![])?;
  conn.execute("drop table project;", params![])?;

  // create tables that use orgauth_user foreign keys and not user.
  conn.execute(
    "CREATE TABLE IF NOT EXISTS \"projectmember\" (\"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
    "CREATE UNIQUE INDEX \"unq\" ON \"projectmember\" (\"project\", \"user\");",
    params![],
  )?;
  conn.execute(
    "CREATE TABLE IF NOT EXISTS \"payentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"paymentdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
    "CREATE UNIQUE INDEX \"payentryunq\" ON \"payentry\" (\"user\", \"paymentdate\");",
    params![],
  )?;
  conn.execute(
    "CREATE TABLE IF NOT EXISTS \"timeentry\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"user\" INTEGER REFERENCES orgauth_user(id) NOT NULL, \"description\" TEXT NOT NULL, \"startdate\" INTEGER NOT NULL, \"enddate\" INTEGER NOT NULL, \"ignore\" BOOLEAN NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
    "CREATE UNIQUE INDEX \"timeentryunq\" ON \"timeentry\" (\"user\", \"startdate\");",
    params![],
  )?;
  conn.execute(
    "CREATE TABLE IF NOT EXISTS \"allocation\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"project\" INTEGER REFERENCES project(id) NOT NULL, \"description\" TEXT NOT NULL, \"duration\" INTEGER NOT NULL, \"allocationdate\" INTEGER NOT NULL, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL, \"creator\" INTEGER REFERENCES orgauth_user(id) NOT NULL);"
    ,params![],
  )?;
  conn.execute(
    "CREATE UNIQUE INDEX \"allocationunq\" ON \"allocation\" (\"user\", \"allocationdate\");",
    params![],
  )?;
  conn.execute(
    "CREATE TABLE IF NOT EXISTS \"project\" (\"id\" INTEGER PRIMARY KEY NOT NULL, \"name\" TEXT NOT NULL, \"description\" TEXT NOT NULL, \"public\" BOOLEAN NOT NULL, \"rate\" REAL, \"currency\" TEXT, \"createdate\" INTEGER NOT NULL, \"changeddate\" INTEGER NOT NULL);"
      ,params![],
    )?;

  // copy out of temp tables.
  conn.execute(
    "insert into projectmember (project, user)
       select project, user from tempprojectmember;",
    params![],
  )?;
  conn.execute(
     "insert into payentry (id, project, user, description, duration, paymentdate, createdate, changeddate, creator)
       select id, project, user, description, duration, paymentdate, createdate, changeddate, creator from temppayentry;"
    ,params![],
  )?;
  conn.execute(
     "insert into timeentry (id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator)
       select id, project, user, description, startdate, enddate, ignore, createdate, changeddate, creator from temptimeentry;"
    ,params![],
  )?;
  conn.execute(
     "insert into allocation (id, project, description, duration, allocationdate, createdate, changeddate, creator)
       select id, project, description, duration, allocationdate, createdate, changeddate, creator from tempallocation;"
    ,params![],
  )?;
  conn.execute(
     "insert into project (id, name, description, public, rate, currency, createdate, changeddate)
       select id, name, description, public, rate, currency, createdate, changeddate from tempproject;"
      ,params![],
    )?;
  // drop old user tables
  let mut m3 = Migration::new();
  m3.drop_table("user");
  m3.drop_table("token");
  m3.drop_table("newemail");
  m3.drop_table("newpassword");
  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  // drop temp tables.
  conn.execute("drop table tempprojectmember;", params![])?;
  conn.execute("drop table temppayentry;", params![])?;
  conn.execute("drop table temptimeentry;", params![])?;
  conn.execute("drop table tempallocation;", params![])?;
  conn.execute("drop table tempproject;", params![])?;
  Ok(())
}

pub fn udpate6(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // back up the projectmember table.

  // temp table to hold data while we make a new table.
  m1.create_table("pmtemp", |t| {
    t.add_column("project", types::integer());
    t.add_column("user", types::integer());
  });

  conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into pmtemp (
      project, user)
     select project, user from projectmember",
    params![],
  )?;

  let mut m2 = Migration::new();
  // drop zknote.
  m2.drop_table("projectmember");

  m2.create_table("projectmember", |t| {
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "orgauth_user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("role", types::text().nullable(false));
    t.add_index("unq", types::index(vec!["project", "user"]).unique(true));
  });

  // add 'rate' to timeentry.
  conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // copy everything from the temp table.
  conn.execute(
    "insert into projectmember (
      project,
      user,
      role)
     select
      project, user, 'Admin' from pmtemp",
    params![],
  )?;

  let mut m3 = Migration::new();
  // drop timeentrytemp.
  m3.drop_table("pmtemp");

  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  Ok(())
}

pub fn udpate7(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  om::udpate2(dbfile)?;
  om::udpate3(dbfile)?;
  om::udpate4(dbfile)?;

  Ok(())
}

pub fn udpate8(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  om::udpate5(dbfile)?;

  Ok(())
}

pub fn udpate9(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  om::udpate6(dbfile)?;

  Ok(())
}

pub fn udpate10(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  om::udpate7(dbfile)?;

  Ok(())
}

pub fn udpate11(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // temp table to hold data while we make a new table.
  m1.create_table("temppayentry", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "orgauth_user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("paymentdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_index(
      "payentryunqtemp",
      types::index(vec!["user", "paymentdate"]).unique(true),
    );
  });

  conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into temppayentry ( id, project, user, description, duration, paymentdate, createdate, changeddate, creator )
     select  id, project, user, description, duration, paymentdate, createdate, changeddate, creator  from payentry",
    params![],
  )?;

  let mut m2 = Migration::new();

  m2.drop_table("payentry");
  m2.create_table("payentry", |t| {
    t.add_column(
      "id",
      types::integer()
        .primary(true)
        .increments(true)
        .nullable(false),
    );
    t.add_column(
      "project",
      types::foreign(
        "project",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column(
      "user",
      types::foreign(
        "orgauth_user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_column("description", types::text().nullable(false));
    t.add_column("duration", types::integer().nullable(false));
    t.add_column("type", types::integer().nullable(false)); // 0 = invoiced  1 = paid
    t.add_column("paymentdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column(
      "creator",
      types::foreign(
        "orgauth_user",
        "id",
        types::ReferentialAction::Restrict,
        types::ReferentialAction::Restrict,
      )
      .nullable(false),
    );
    t.add_index(
      "payentryunq",
      types::index(vec!["user", "paymentdate"]).unique(true),
    );
  });

  // add 'rate' to timeentry.
  conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // copy everything from the temp table.
  conn.execute(
    "insert into payentry ( id, project, user, description, duration, type, paymentdate, createdate, changeddate, creator )
     select  id, project, user, description, duration, 1, paymentdate, createdate, changeddate, creator  from temppayentry",
    params![],
  )?;

  // also create invoice records for all payments.
  conn.execute(
    "insert into payentry ( project, user, description, duration, type, paymentdate, createdate, changeddate, creator )
     select  project, user, description, duration, 0, paymentdate - 1, createdate, changeddate, creator  from temppayentry",
    params![],
  )?;

  let mut m3 = Migration::new();
  // drop temp table..
  m3.drop_table("temppayentry");

  conn.execute_batch(m3.make::<Sqlite>().as_str())?;

  Ok(())
}

pub fn udpate12(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;
  let mut m1 = Migration::new();

  // add 'rate' to timeentry.
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
    t.add_column("rate", types::float().nullable(true));
    t.add_column("currency", types::text().nullable(true));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
  });

  conn.execute_batch(m1.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into projecttemp (  id, name, description, public, rate, currency, createdate, changeddate)
     select id, name, description, public, rate, currency, createdate, changeddate from project ",
    params![],
  )?;

  let mut m2 = Migration::new();
  m2.drop_table("project");
  // timeclonk specific tables.
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
    t.add_column("due_days", types::integer().nullable(true));
    t.add_column("extra_fields", types::text().nullable(true));
    t.add_column("invoice_id_template", types::text().nullable(false));
    t.add_column("invoice_seq", types::integer().nullable(false));
    t.add_column("payer", types::text().nullable(false));
    t.add_column("payee", types::text().nullable(false));
    t.add_column("generic_task", types::text().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
  });

  conn.execute_batch(m2.make::<Sqlite>().as_str())?;

  // copy everything from current table..
  conn.execute(
    "insert into project (id, name, description, public, rate, currency, invoice_id_template ,invoice_seq, payer, payee, generic_task, createdate, changeddate)
     select id, name, description, public, rate, currency, '', 0, '', '', '', createdate, changeddate from projecttemp ",
    params![],
  )?;

  Ok(())
}

pub fn udpate13(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;

  // drop the work table from the last migration.
  conn.execute("drop table projecttemp ", params![])?;

  Ok(())
}


// {
//   // create backup table for project.
//   let mut m1 = Migration::new();

//   m1.create_table("projecttemp", |t| {
//     t.add_column(
//       "id",
//       types::integer()
//         .primary(true)
//         .increments(true)
//         .nullable(false),
//     );
//     t.add_column("name", types::text().nullable(false));
//     t.add_column("description", types::text().nullable(false));
//     t.add_column("public", types::boolean().nullable(false));
//     t.add_column("rate", types::float().nullable(true));
//     t.add_column("currency", types::text().nullable(true));
//     t.add_column("due_days", types::integer().nullable(true));
//     t.add_column("extra_fields", types::text().nullable(true));
//     t.add_column("invoice_id_template", types::text().nullable(false));
//     t.add_column("invoice_seq", types::integer().nullable(false));
//     t.add_column("payer", types::text().nullable(false));
//     t.add_column("payee", types::text().nullable(false));
//     t.add_column("generic_task", types::text().nullable(false));
//     t.add_column("createdate", types::integer().nullable(false));
//     t.add_column("changeddate", types::integer().nullable(false));
//   });

//   conn.execute_batch(m1.make::<Sqlite>().as_str())?;

//   // copy project table.
//   conn.execute(
//     "insert into projecttemp (id, name, description, public, rate, currency, due_days, extra_fields, invoice_id_template, invoice_seq, payer, payee, generic_task, createdate, changeddate)
//      select id, name, description, public, rate, currency, due_days, extra_fields, invoice_id_template, invoice_seq, payer, payee, generic_task, createdate, changeddate from project ",
//     params![],
//   )?;
  

//   let mut m2 = Migration::new();
//   m2.drop_table("project");
//   // timeclonk specific tables.
//   m2.create_table("client", |t| {
//     t.add_column(
//       "id",
//       types::integer()
//         .primary(true)
//         .increments(true)
//         .nullable(false),
//     );
//     t.add_column("name", types::text().nullable(false));
//     t.add_column("description", types::text().nullable(false));
//     t.add_column("createdate", types::integer().nullable(false));
//     t.add_column("changeddate", types::integer().nullable(false));
//   });

//   m2.create_table("sow", |t| {
//     t.add_column(
//       "id",
//       types::integer()
//         .primary(true)
//         .increments(true)
//         .nullable(false),
//     );
//     t.add_column("name", types::text().nullable(false));
//     t.add_column("description", types::text().nullable(false));
//     t.add_column("startdate", types::integer().nullable(true));
//     t.add_column("enddate", types::integer().nullable(true));
//     t.add_column("public", types::boolean().nullable(false));
//     t.add_column("rate", types::float().nullable(true));
//     t.add_column("currency", types::text().nullable(true));
//     t.add_column("due_days", types::integer().nullable(true));
//     t.add_column("extra_fields", types::text().nullable(true));
//     t.add_column("invoice_id_template", types::text().nullable(false));
//     t.add_column("invoice_seq", types::integer().nullable(false));
//     t.add_column("payer", types::text().nullable(false));
//     t.add_column("payee", types::text().nullable(false));
//     t.add_column("generic_task", types::text().nullable(false));
//     t.add_column("createdate", types::integer().nullable(false));
//     t.add_column("changeddate", types::integer().nullable(false));
//   });


//   Ok(())
// }



pub fn udpate14(dbfile: &Path) -> Result<(), orgauth::error::Error> {
  // db connection without foreign key checking.
  let conn = Connection::open(dbfile)?;

  // drop the work table from the last migration.
  conn.execute("drop table projecttemp ", params![])?;

  Ok(())
}

DROP INDEX "timeentryunq";
DROP INDEX "allocationunq";
DROP INDEX "unq";
DROP INDEX "payentryunq";
// rename all timeclonk tables
ALTER TABLE "timeentry" RENAME TO "timeentry_bk";
ALTER TABLE "allocation" RENAME TO "allocation_bk";
ALTER TABLE "projectmember" RENAME TO "projectmember_bk";
ALTER TABLE "payentry" RENAME TO "payentry_bk";
ALTER TABLE "project" RENAME TO "project_bk";

// new timeclonk tables.
CREATE TABLE IF NOT EXISTS "timeentry" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "sow" INTEGER REFERENCES sow
    (id) NOT NULL,
   "user" INTEGER REFERENCES sowmember
    (id) NOT NULL,
   "description" TEXT NOT NULL,
   "startdate" INTEGER NOT NULL,
   "enddate" INTEGER NOT NULL,
   "ignore" BOOLEAN NOT NULL,
   "createdate" INTEGER NOT NULL,
   "changeddate" INTEGER NOT NULL,
   "creator" INTEGER REFERENCES orgauth_user
    (id) NOT NULL);
CREATE UNIQUE INDEX "timeentryunq" ON "timeentry" 
  ("user",
 o  "startdate");
CREATE TABLE IF NOT EXISTS "clientmember" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "client" INTEGER REFERENCES client(id) NOT NULL,
   "user" INTEGER REFERENCES orgauth_user
    (id) NOT NULL,
   "role" TEXT NOT NULL);
CREATE UNIQUE INDEX "projectmemberunq" ON "projectmember" 
  ("project",
   "user");
CREATE TABLE IF NOT EXISTS "sowmember" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "sow" INTEGER REFERENCES sow(id) NOT NULL,
   "clientmember" INTEGER NOT NULL REFERENCES clientmember
    (id) ON UPDATE RESTRICT ON DELETE RESTRICT,
   "role" TEXT NOT NULL);
CREATE UNIQUE INDEX "sowmemberunq" ON "sowmember" 
  ("sow",
   "projectmember");
CREATE TABLE IF NOT EXISTS "payentry" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "sow" INTEGER NOT NULL REFERENCES sow
    (id) ON UPDATE RESTRICT ON DELETE RESTRICT,
   "user" INTEGER NOT NULL REFERENCES orgauth_user
    (id) ON UPDATE RESTRICT ON DELETE RESTRICT,
   "description" TEXT NOT NULL,
   "duration" INTEGER NOT NULL,
   "type" INTEGER NOT NULL,
   "paymentdate" INTEGER NOT NULL,
   "createdate" INTEGER NOT NULL,
   "changeddate" INTEGER NOT NULL,
   "creator" INTEGER NOT NULL REFERENCES orgauth_user
    (id) ON UPDATE RESTRICT ON DELETE RESTRICT);
CREATE UNIQUE INDEX "payentryunq" ON "payentry" 
  ("user",
   "paymentdate");
CREATE TABLE IF NOT EXISTS "client" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "name" TEXT NOT NULL,
   "description" TEXT NOT NULL,
   "invoice_id_template" TEXT NOT NULL,
   "invoice_seq" INTEGER NOT NULL,
   "payer" TEXT NOT NULL,
   "payee" TEXT NOT NULL,
   "creator" INTEGER REFERENCES orgauth_user (id) NOT NULL;
   "createdate" INTEGER NOT NULL,
   "changeddate" INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS "sow" 
  ("id" INTEGER PRIMARY KEY NOT NULL,
   "name" TEXT NOT NULL,
   "description" TEXT NOT NULL,
   "startdate" INTEGER,
   "enddate" INTEGER,
   "duration" INTEGER NOT NULL,
   "closed" BOOLEAN NOT NULL,
   "public" BOOLEAN NOT NULL,
   "rate" REAL,
   "currency" TEXT,
   "due_days" INTEGER,
   "extra_fields" TEXT,
   "invoice_id_template" TEXT NOT NULL,
   "invoice_seq" INTEGER NOT NULL,
   "payer" TEXT NOT NULL,
   "payee" TEXT NOT NULL,
   "generic_task" TEXT NOT NULL,
   "creator" INTEGER REFERENCES clientmember (id) NOT NULL;
   "createdate" INTEGER NOT NULL,
   "changeddate" INTEGER NOT NULL);

// copy in old timeclonk data.  

// make clients from projects
// make sows from allocations and projects.
// copy timeentries, assign to sows by start date.

insert into client (id, name, description, invoice_id_template, invoice_seq, payer, payee, creator, createdate, changeddate)
select id name, description, invoice_id_template, invoice_seq, createdate, changeddate
from project_bk;

// insert into sow (id, name, description, startdate, enddate, duration, closed, public, rate, currency, due_days, extra_fields, invoice_id_template, invoice_seq, payer, payee, generic_task, creator, createdate, changeddate)
// select p.id, p.name, p.description, p.public, p.rate, p.currency, p.due_days, p.extra_fields, p.invoice_id_template, p.invoice_seq, p.payer, p.payee, p.generic_task, p.createdate, p.changeddate
// a.id, a.project, a.description, a.duration, a.allocationdate, a.createdate, a.changeddate, a.creator
// from project p, allocation a
// where a.project = p.id

// enddate will be null on these.
insert into sow (id, name, description, startdate, duration, closed, public, rate, currency, due_days, extra_fields, payer, payee, generic_task, creator, createdate, changeddate)
select a.id, p.name, a.description, a.allocationdate, a.duration, false, p.public, p.rate, p.currency, p.due_days, p.extra_fields, p.payer, p.payee, p.generic_task, a.creator, a.createdate, a.changeddate
from project_bk p, allocation_bk a
where a.project = p.id;

insert into clientmember (id, client, user, role)
select from projectmember  project, user, role


// TODO: rename client to project again.
// Q: do we want to use the fancy sowmamber id to enforce project membership, or just use orgauth_ids.  



// Q: what about the use case where you want a single link to a public project?  If new sows are issued you don't have to update the link.


// Q; have a rate, currency, etc in the project?  to copy to new sows.

