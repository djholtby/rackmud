CREATE TABLE classes (
	cid INTEGER PRIMARY KEY,
	classname text NOT NULL,
	module text NOT NULL
);

CREATE TABLE objects (
	oid INTEGER PRIMARY KEY AUTOINCREMENT,
	cid INTEGER NOT NULL,
	created NOT NULL,
	saved,
	name text NOT NULL DEFAULT 'object',
	deleted INTEGER NOT NULL default 0,
	fields BLOB,
	FOREIGN KEY(cid) REFERENCES classes(cid)
);


CREATE TABLE singletons (
   cid INTEGER UNIQUE NOT NULL,
   oid INTEGER UNIQUE NOT NULL,
   PRIMARY KEY(cid),
   FOREIGN KEY(cid) REFERENCES classes(cid),
   FOREIGN KEY(oid) REFERENCES objects(oid)
);

CREATE TABLE tags (
   oid INTEGER NOT NULL,
   category TEXT NOT NULL,
   tag TEXT NOT NULL,
   PRIMARY KEY (oid,category,tag),
   FOREIGN KEY(oid) REFERENCES objects(oid)
);


CREATE TABLE indexed_fields (
   oid INTEGER NOT NULL,
   field TEXT NOT NULL,
   value BLOB NOT NULL,
   PRIMARY KEY (oid, field),
   FOREIGN KEY(oid) REFERENCES objects(oid)
);   

CREATE TABLE logfile (
  time NOT NULL,
  level INTEGER NOT NULL,
  module text,
  description text,
  backtrace text
);

CREATE TABLE localization (
  tid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  lang CHARACTER(3) NOT NULL default 'eng',
  enc SMALLINT NOT NULL default 3,
  txt text NOT NULL
);


CREATE TABLE auth (
   seq TEXT NOT NULL,
   token CHARACTER(64) NOT NULL,
   oid bigint NOT NULL,
   expires timestamp NOT NULL,
   PRIMARY KEY(seq),
   FOREIGN KEY(oid) REFERENCES objects(oid)

);  


CREATE TABLE metadata (
  id text NOT NULL UNIQUE,
  val text NOT NULL,
  PRIMARY KEY(id)
);

INSERT INTO metadata values ('database-version', '1');

CREATE INDEX auth_by_seq ON auth (seq);
CREATE INDEX auth_by_oid ON auth (oid);


CREATE INDEX loc_by_textlang ON localization (tid, lang);
CREATE INDEX loc_by_textlangenc ON localization (tid, lang, enc);
CREATE INDEX loc_by_lang ON localization (lang, enc, txt);



CREATE INDEX log_by_time on logfile (time);
CREATE INDEX log_by_level on logfile (level, time);
CREATE INDEX log_by_module on logfile (module, time);

CREATE INDEX tag_by_obj ON tags (oid);

CREATE INDEX obj_by_tag ON tags (category,tag);

CREATE INDEX fields_by_obj ON indexed_fields (field, value);
