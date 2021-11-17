use crate::data::{
  ChangeEmail, ChangePassword, ListProject, LoginData, Project, ProjectEdit, ProjectMember,
  SaveProject, SavedProject, User,
};
use crate::util::{is_token_expired, now};
use barrel::backend::Sqlite;
use barrel::{types, Migration};
use crypto_hash::{hex_digest, Algorithm};
use log::info;
use rusqlite::{params, Connection};
use simple_error::bail;
use std::error::Error;
use std::path::Path;
use std::time::Duration;
use uuid::Uuid;

pub fn login_data(conn: &Connection, uid: i64) -> Result<LoginData, Box<dyn Error>> {
  let user = read_user_by_id(&conn, uid)?;
  Ok(LoginData {
    userid: uid,
    name: user.name,
  })
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
    t.add_column("paymentdate", types::integer().nullable(false));
    t.add_column("createdate", types::integer().nullable(false));
    t.add_column("changeddate", types::integer().nullable(false));
    t.add_column("creator", types::foreign("user", "id").nullable(false));
  });

  m
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

  info!("db up to date.");

  purge_login_tokens(&conn, token_expiration_ms)?;

  Ok(())
}

// user CRUD

pub fn new_user(
  dbfile: &Path,
  name: String,
  hashwd: String,
  salt: String,
  email: String,
  registration_key: String,
) -> Result<i64, Box<dyn Error>> {
  let conn = connection_open(dbfile)?;

  // let usernoteid = note_id(&conn, "system", "user")?;
  // let publicnoteid = note_id(&conn, "system", "public")?;
  // let systemid = user_id(&conn, "system")?;

  let now = now()?;

  // make a user record.
  conn.execute(
    "insert into user (name, hashwd, salt, email, registration_key, createdate)
      values (?1, ?2, ?3, ?4, ?5, ?6)",
    params![name, hashwd, salt, email, registration_key, now],
  )?;

  let uid = conn.last_insert_rowid();

  Ok(uid)
}

pub fn read_user_by_name(conn: &Connection, name: &str) -> Result<User, Box<dyn Error>> {
  let user = conn.query_row(
    "select id, hashwd, salt, email, registration_key
      from user where name = ?1",
    params![name],
    |row| {
      Ok(User {
        id: row.get(0)?,
        name: name.to_string(),
        hashwd: row.get(1)?,
        salt: row.get(2)?,
        email: row.get(3)?,
        registration_key: row.get(4)?,
      })
    },
  )?;

  Ok(user)
}

pub fn read_user_by_id(conn: &Connection, id: i64) -> Result<User, Box<dyn Error>> {
  let user = conn.query_row(
    "select id, name, hashwd, salt, email, registration_key
      from user where id = ?1",
    params![id],
    |row| {
      Ok(User {
        id: row.get(0)?,
        name: row.get(1)?,
        hashwd: row.get(2)?,
        salt: row.get(3)?,
        email: row.get(4)?,
        registration_key: row.get(5)?,
      })
    },
  )?;

  Ok(user)
}

pub fn read_user_by_token(
  conn: &Connection,
  token: Uuid,
  token_expiration_ms: Option<i64>,
) -> Result<User, Box<dyn Error>> {
  let (user, tokendate) = conn.query_row(
    "select id, name, hashwd, salt, email, registration_key, token.tokendate
      from user, token where user.id = token.user and token = ?1",
    params![token.to_string()],
    |row| {
      Ok((
        User {
          id: row.get(0)?,
          name: row.get(1)?,
          hashwd: row.get(2)?,
          salt: row.get(3)?,
          email: row.get(4)?,
          registration_key: row.get(5)?,
        },
        row.get(6)?,
      ))
    },
  )?;

  match token_expiration_ms {
    Some(texp) => {
      if is_token_expired(texp, tokendate) {
        bail!("login expired")
      } else {
        Ok(user)
      }
    }
    None => Ok(user),
  }
}

pub fn add_token(conn: &Connection, user: i64, token: Uuid) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  conn.execute(
    "insert into token (user, token, tokendate)
     values (?1, ?2, ?3)",
    params![user, token.to_string(), now],
  )?;

  Ok(())
}

pub fn purge_login_tokens(
  conn: &Connection,
  token_expiration_ms: i64,
) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  let expdt = now - token_expiration_ms;

  let count: i64 = conn.query_row(
    "select count(*) from
      token where tokendate < ?1",
    params![expdt],
    |row| Ok(row.get(0)?),
  )?;

  if count > 0 {
    info!("removing {} expired token records", count);

    conn.execute(
      "delete from token
        where tokendate < ?1",
      params![expdt],
    )?;
  }

  Ok(())
}

pub fn purge_email_tokens(
  conn: &Connection,
  token_expiration_ms: i64,
) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  let expdt = now - token_expiration_ms;

  let count: i64 = conn.query_row(
    "select count(*) from
      newemail where tokendate < ?1",
    params![expdt],
    |row| Ok(row.get(0)?),
  )?;

  if count > 0 {
    info!("removing {} expired newemail records", count);

    conn.execute(
      "delete from newemail
        where tokendate < ?1",
      params![expdt],
    )?;
  }

  Ok(())
}

pub fn purge_reset_tokens(
  conn: &Connection,
  token_expiration_ms: i64,
) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  let expdt = now - token_expiration_ms;

  let count: i64 = conn.query_row(
    "select count(*) from
      newpassword where tokendate < ?1",
    params![expdt],
    |row| Ok(row.get(0)?),
  )?;

  if count > 0 {
    info!("removing {} expired newpassword records", count);

    conn.execute(
      "delete from newpassword
        where tokendate < ?1",
      params![expdt],
    )?;
  }

  Ok(())
}

pub fn update_user(conn: &Connection, user: &User) -> Result<(), Box<dyn Error>> {
  conn.execute(
    "update user set name = ?1, hashwd = ?2, salt = ?3, email = ?4, registration_key = ?5
           where id = ?6",
    params![
      user.name,
      user.hashwd,
      user.salt,
      user.email,
      user.registration_key,
      user.id,
    ],
  )?;

  Ok(())
}

// email change request.
pub fn add_newemail(
  conn: &Connection,
  user: i64,
  token: Uuid,
  email: String,
) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  conn.execute(
    "insert into newemail (user, email, token, tokendate)
     values (?1, ?2, ?3, ?4)",
    params![user, email, token.to_string(), now],
  )?;

  Ok(())
}

// email change request.
pub fn read_newemail(
  conn: &Connection,
  user: i64,
  token: Uuid,
) -> Result<(String, i64), Box<dyn Error>> {
  let result = conn.query_row(
    "select email, tokendate from newemail
     where user = ?1
      and token = ?2",
    params![user, token.to_string()],
    |row| Ok((row.get(0)?, row.get(1)?)),
  )?;
  Ok(result)
}

// email change request.
pub fn remove_newemail(conn: &Connection, user: i64, token: Uuid) -> Result<(), Box<dyn Error>> {
  conn.execute(
    "delete from newemail
     where user = ?1 and token = ?2",
    params![user, token.to_string()],
  )?;

  Ok(())
}

// password reset request.
pub fn add_newpassword(conn: &Connection, user: i64, token: Uuid) -> Result<(), Box<dyn Error>> {
  let now = now()?;
  conn.execute(
    "insert into newpassword (user, token, tokendate)
     values (?1, ?2, ?3)",
    params![user, token.to_string(), now],
  )?;

  Ok(())
}

// password reset request.
pub fn read_newpassword(conn: &Connection, user: i64, token: Uuid) -> Result<i64, Box<dyn Error>> {
  let result = conn.query_row(
    "select tokendate from newpassword
     where user = ?1
      and token = ?2",
    params![user, token.to_string()],
    |row| Ok(row.get(0)?),
  )?;
  Ok(result)
}

// password reset request.
pub fn remove_newpassword(conn: &Connection, user: i64, token: Uuid) -> Result<(), Box<dyn Error>> {
  conn.execute(
    "delete from newpassword
     where user = ?1 and token = ?2",
    params![user, token.to_string()],
  )?;

  Ok(())
}

pub fn user_id(conn: &Connection, name: &str) -> Result<i64, Box<dyn Error>> {
  let id: i64 = conn.query_row(
    "select id from user
      where user.name = ?1",
    params![name],
    |row| Ok(row.get(0)?),
  )?;
  Ok(id)
}

pub fn change_password(
  conn: &Connection,
  uid: i64,
  cp: ChangePassword,
) -> Result<(), Box<dyn Error>> {
  let mut userdata = read_user_by_id(&conn, uid)?;
  match userdata.registration_key {
    Some(_reg_key) => bail!("invalid user or password"),
    None => {
      if hex_digest(
        Algorithm::SHA256,
        (cp.oldpwd.clone() + userdata.salt.as_str())
          .into_bytes()
          .as_slice(),
      ) != userdata.hashwd
      {
        // bad password, can't change.
        bail!("invalid password!")
      } else {
        let newhash = hex_digest(
          Algorithm::SHA256,
          (cp.newpwd.clone() + userdata.salt.as_str())
            .into_bytes()
            .as_slice(),
        );
        userdata.hashwd = newhash;
        update_user(&conn, &userdata)?;
        info!("changed password for {}", userdata.name);

        Ok(())
      }
    }
  }
}

pub fn change_email(
  conn: &Connection,
  uid: i64,
  cp: ChangeEmail,
) -> Result<(String, Uuid), Box<dyn Error>> {
  let userdata = read_user_by_id(&conn, uid)?;
  match userdata.registration_key {
    Some(_reg_key) => bail!("invalid user or password"),
    None => {
      if hex_digest(
        Algorithm::SHA256,
        (cp.pwd.clone() + userdata.salt.as_str())
          .into_bytes()
          .as_slice(),
      ) != userdata.hashwd
      {
        // bad password, can't change.
        bail!("invalid password!")
      } else {
        // create a 'newemail' record.
        let token = Uuid::new_v4();
        add_newemail(&conn, uid, token, cp.email)?;

        Ok((userdata.name, token))
      }
    }
  }
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
pub fn save_project(
  conn: &Connection,
  user: i64,
  project: SaveProject,
) -> Result<SavedProject, Box<dyn Error>> {
  let now = now()?;

  let proj = match project.id {
    Some(id) => {
      conn.execute(
        "update project set name = ?1, description = ?2, public = ?3, changeddate = ?4
          where id = ?5",
        params![project.name, project.description, project.public, now, id],
      )?;
      SavedProject {
        id: id,
        changeddate: now,
      }
    }
    None => {
      conn.execute(
        "insert into project (name, description, public, createdate, changeddate)
         values (?1, ?2, ?3, ?4, ?5)",
        params![project.name, project.description, project.public, now, now],
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
    "select project.id, project.name, project.description, project.public, project.createdate, project.changeddate
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
      createdate: row.get(4)?,
      changeddate: row.get(5)?,
    })
  })?);
  r
}

pub fn member_list(
  conn: &Connection,
  uid: i64,
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
        .query_map(params![uid], |row| {
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
