PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE VIRTUAL TABLE IF NOT EXISTS "talk" USING fts4
  ("id" INTEGER PRIMARY KEY
  ,"title" VARCHAR NULL
  ,"abstract" VARCHAR NULL
  ,"link" VARCHAR NOT NULL
  ,"speaker_name" VARCHAR NULL
  ,"speaker_id" INTEGER NULL REFERENCES "speaker"
  ,"workshop_id" INTEGER NULL REFERENCES "workshop"
  );

CREATE TABLE IF NOT EXISTS "speaker"
  ("id" INTEGER PRIMARY KEY
  ,"name" VARCHAR NOT NULL
  ,"last_name" VARCHAR NOT NULL,CONSTRAINT "speaker_name" UNIQUE ("name")
  );

CREATE TABLE IF NOT EXISTS "workshop"
  ("id" INTEGER PRIMARY KEY
  ,"title" VARCHAR NOT NULL,CONSTRAINT "unique_title" UNIQUE ("title")
  );

COMMIT;
