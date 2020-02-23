CREATE TABLE classes (
	cid serial,
	classname text NOT NULL,
	module text NOT NULL,
	PRIMARY KEY (cid)
);

CREATE TABLE objects (
	oid BIGSERIAL UNIQUE ,
	cid int NOT NULL REFERENCES classes,
	created timestamp NOT NULL,
	saved timestamp,
	deleted bool NOT NULL default false,
	fields bytea,
	PRIMARY KEY (oid)
);


CREATE TABLE singletons (
   cid int UNIQUE NOT NULL REFERENCES classes,
   oid bigint UNIQUE NOT NULL REFERENCES objects,
   PRIMARY KEY(cid)
);

CREATE TABLE tags (
   oid bigint NOT NULL REFERENCES objects,
   category text NOT NULL,
   tag text NOT NULL,
   PRIMARY KEY (oid,category,tag)
);

CREATE INDEX ON tags (oid);
CREATE INDEX ON tags (category,tag);

CREATE TABLE indexed_fields (
   oid bigint NOT NULL REFERENCES objects,
   field text NOT NULL,
   value bytea NOT NULL,
   PRIMARY KEY (oid, field)
);   

CREATE INDEX ON indexed_fields (field, value);
CREATE INDEX ON indexed_fields (oid);

CREATE TABLE logfile (
  time timestamp NOT NULL default NOW(),
  level smallint NOT NULL,
  module text,
  description text,
  backtrace text
);

CREATE INDEX on logfile (time);
CREATE INDEX on logfile (level, time);
CREATE INDEX on logfile (module, time);


CREATE TABLE localization (
  tid serial NOT NULL PRIMARY KEY,
  lang char(3) NOT NULL default 'eng',
  enc SMALLINT NOT NULL default 3,
  txt text NOT NULL
);


CREATE INDEX ON localization (tid, lang);
CREATE INDEX ON localization (tid, lang, enc);
CREATE INDEX ON localization (lang, enc, txt);

CREATE TABLE auth (
   seq uuid NOT NULL,
   token char(64) NOT NULL,
   oid bigint NOT NULL REFERENCES objects,
   expires timestamp NOT NULL,
   PRIMARY KEY(seq)
);  

CREATE INDEX ON auth (seq);
CREATE INDEX ON auth (oid);

CREATE TABLE metadata (
  id text NOT NULL UNIQUE,
  val text NOT NULL,
  PRIMARY KEY(id)
);

INSERT INTO metadata values ('database-version', '1');
   
