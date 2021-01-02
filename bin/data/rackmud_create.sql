CREATE TABLE classes (
	cid SERIAL UNIQUE NOT NULL,
	classname TEXT NOT NULL,
	module TEXT NOT NULL,
	PRIMARY KEY (cid)
);

CREATE UNIQUE INDEX classes_lookup ON classes (module, classname);

CREATE OR REPLACE FUNCTION get_cid(TEXT, TEXT) RETURNS INT AS
$$
DECLARE 
	res INT;
BEGIN
	SELECT cid INTO res FROM classes c WHERE c.module = $1 AND c.classname = $2;
	IF NOT FOUND THEN
		INSERT INTO classes (module, classname) VALUES ($1, $2) RETURNING cid INTO res;
	END IF;
	RETURN res;
END;
$$
LANGUAGE plpgsql;

CREATE TABLE objects (
	oid BIGSERIAL UNIQUE NOT NULL,
	cid INT NOT NULL REFERENCES classes,
	created TIMESTAMPTZ NOT NULL DEFAULT now(),
	saved TIMESTAMPTZ,
	PRIMARY KEY (oid)
);

CREATE INDEX objects_by_ver ON objects (saved);

CREATE TABLE field_index_names (
	fid SERIAL UNIQUE NOT NULL,
	cid INT NOT NULL REFERENCES classes,
	field_name TEXT NOT NULL,
	PRIMARY KEY (fid)
);

CREATE UNIQUE INDEX field_id_index ON field_index_names USING btree(cid, field_name);






CREATE OR REPLACE FUNCTION get_field_index(INT, VARCHAR) RETURNS INT AS
$$
DECLARE
  res INT;
BEGIN
  SELECT fid INTO res FROM field_index_names f WHERE f.cid = $1 AND f.field_name = $2;
  IF NOT FOUND THEN 
    INSERT INTO field_index_names (cid, field_name) VALUES ($1, $2) RETURNING fid INTO res;
  END IF;
  RETURN res;
END;
$$
LANGUAGE plpgsql;

CREATE TABLE object_fields (
       oid BIGINT REFERENCES objects,
       saved TIMESTAMPTZ NOT NULL DEFAULT now(),
       fields JSONB,
       PRIMARY KEY (oid, saved)
);

CREATE INDEX object_by_tags ON object_fields USING gin((fields->'tags'->'value'));

CREATE OR REPLACE FUNCTION object_version_update() RETURNS TRIGGER AS                                                                                                               
$$
BEGIN
    UPDATE objects SET saved = new.saved WHERE oid = new.oid;
    RETURN new;
END;
$$
LANGUAGE plpgsql;

CREATE TRIGGER trig_version_update AFTER INSERT ON object_fields 
       FOR EACH ROW EXECUTE PROCEDURE object_version_update();



CREATE TABLE singletons (
   cid INT UNIQUE NOT NULL REFERENCES classes,
   oid BIGINT UNIQUE NOT NULL REFERENCES objects,
   PRIMARY KEY(cid)
);

CREATE TABLE logfile (
  time TIMESTAMPTZ NOT NULL default NOW(),
  level SMALLINT NOT NULL,
  module TEXT,
  description TEXT,
  backtrace TEXT
);

CREATE INDEX on logfile (time desc);
CREATE INDEX on logfile (level);
CREATE INDEX on logfile (module);

CREATE TABLE auth (
   seq UUID NOT NULL DEFAULT uuid_generate_v1mc(),
   sig BYTEA NOT NULL,
   oid BIGINT NOT NULL REFERENCES objects,
   issued TIMESTAMPTZ NOT NULL DEFAULT now(),
   expires TIMESTAMPTZ,
   session BOOLEAN NOT NULL DEFAULT false,
   PRIMARY KEY(seq)
);  

CREATE INDEX ON auth USING btree(oid);
CREATE INDEX ON auth USING btree(expires) WHERE expires IS NOT NULL;

CREATE TABLE metadata (
  id TEXT NOT NULL UNIQUE,
  val JSONB NOT NULL,
  PRIMARY KEY(id)
);

INSERT INTO metadata VALUES ('database-version', '2'::jsonb);
