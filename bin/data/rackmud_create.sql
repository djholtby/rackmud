CREATE TABLE classes (
	cid SERIAL UNIQUE NOT NULL,
    ancestors INT[] NOT NULL DEFAULT '{}'::int[],
	classname TEXT NOT NULL,
	module TEXT NOT NULL,
	PRIMARY KEY (cid)
);

CREATE UNIQUE INDEX classes_lookup ON classes (module, classname);
CREATE INDEX classes_ancestors ON classes USING GIN(ancestors gin__int_ops);

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
CREATE INDEX objects_by_class ON objects (cid);

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


CREATE TABLE snapshot(
   sid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
   freq BOOLEAN NOT NULL DEFAULT true,
   hourly BOOLEAN NOT NULL DEFAULT false,
   daily BOOLEAN NOT NULL DEFAULT false,
   weekly BOOLEAN NOT NULL DEFAULT false,
   monthly BOOLEAN NOT NULL DEFAULT false,
   yearly BOOLEAN NOT NULL DEFAULT false,
   taken TIMESTAMPTZ NOT NULL DEFAULT now()
);




CREATE TABLE object_fields (
       oid BIGINT REFERENCES objects,
       saved TIMESTAMPTZ NOT NULL DEFAULT now(),
       fields JSONB,
	   PRIMARY KEY (oid, saved)
);

ALTER TABLE objects ADD FOREIGN KEY (oid, saved) REFERENCES object_fields (oid, saved) ON UPDATE CASCADE ON DELETE RESTRICT;
CREATE INDEX object_fkey_index ON objects USING btree(oid,saved);

CREATE TABLE snapshot_fields (
  sid INT NOT NULL REFERENCES snapshot ON DELETE CASCADE,
  oid BIGINT NOT NULL,
  saved TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (sid, oid),
  FOREIGN KEY (oid, saved) REFERENCES object_fields (oid, saved) ON UPDATE CASCADE ON DELETE RESTRICT;
);

CREATE UNIQUE INDEX snapshot_by_time ON snapshot USING btree(taken);
CREATE INDEX snapshot_fields_fkey_index ON snapshot_fields USING btree(oid,saved);


CREATE FUNCTION take_snapshot(boolean, boolean, boolean, boolean, boolean, boolean) RETURNS TIMESTAMPTZ AS
$$
DECLARE 
  new_sid INT;
  taken_value TIMESTAMPTZ;
BEGIN
  INSERT INTO snapshot (freq, hourly, daily, weekly, monthly, yearly) VALUES ($1, $2, $3, $4, $5, $6) RETURNING sid,taken INTO new_sid,taken_value;
  INSERT INTO snapshot_fields (sid, oid, saved) 
    SELECT new_sid, objects.oid, objects.saved FROM objects; 
   
  return taken_value;
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION object_upsert_fields(BIGINT, JSONB) RETURNS TIMESTAMPTZ AS
$$
DECLARE
  old_fields JSONB;
  ts TIMESTAMPTZ;
BEGIN
  SELECT od.fields, o.saved into old_fields, ts FROM objects o INNER JOIN object_fields od 
   ON (o.oid = $1) AND (od.oid = $1) AND (o.saved = od.saved);
  IF old_fields = $2 THEN
	 UPDATE object_fields SET saved = now() WHERE oid = $1 AND saved = ts;
     RETURN ts;
  ELSE
     INSERT INTO object_fields (oid, fields) values ($1, $2) ON CONFLICT (oid, saved) DO UPDATE SET fields = EXCLUDED.fields RETURNING saved into ts;  
	 RETURN ts;
  END IF;
END;
$$
LANGUAGE plpgsql;


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

CREATE INDEX ON logfile USING btree(time desc);
CREATE INDEX ON logfile USING btree(module);

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

CREATE TABLE jwt_revoke (
   jwt TEXT NOT NULL,
   expires TIMESTAMPTZ,
   PRIMARY KEY(jwt)
);

CREATE INDEX ON jwt_revoke using btree(expires);

CREATE TABLE metadata (
  id TEXT NOT NULL UNIQUE,
  val JSONB NOT NULL,
  PRIMARY KEY(id)
);

INSERT INTO metadata VALUES ('database-version', '3'::jsonb);
