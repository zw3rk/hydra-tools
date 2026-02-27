CREATE TABLE IF NOT EXISTS DrvpathsToUpload (
    id SERIAL PRIMARY KEY,
    drvpath TEXT NOT NULL,
    last TIMESTAMP DEFAULT NOW(),
    tries INTEGER DEFAULT 0
);

-- Deduplicate the queue: only one entry per store path.  Multiple
-- triggers (BuildSteps, BuildStepOutputs, Builds, BuildOutputs) can
-- fire for the same path; without this constraint, duplicate rows
-- cause deadlocks when workers concurrently DELETE by drvpath.
CREATE UNIQUE INDEX IF NOT EXISTS drvpaths_to_upload_drvpath_idx
  ON DrvpathsToUpload (drvpath);

CREATE OR REPLACE FUNCTION process_build_step_update()
RETURNS trigger AS $$
BEGIN
  IF NEW.status = 0 AND OLD.status is NULL THEN
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.drvpath)
        ON CONFLICT (drvpath) DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER build_step_update_trigger
AFTER UPDATE ON BuildSteps
FOR EACH ROW
WHEN (NEW.status = 0 AND OLD.status is NULL)
EXECUTE PROCEDURE process_build_step_update();

-- Also ensure we capture all build-products that hydra sees.

CREATE OR REPLACE FUNCTION process_build_step_output()
RETURNS trigger AS $$
BEGIN
  IF NEW.path is not NULL THEN
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.path)
        ON CONFLICT (drvpath) DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER build_step_output_trigger
AFTER INSERT OR UPDATE ON BuildStepOutputs
FOR EACH ROW
WHEN (NEW.path is not NULL)
EXECUTE PROCEDURE process_build_step_output();

-- Just in cast no build steps were needed.
-- This can happen if the derivation was built by something
-- other than hydra already.
CREATE OR REPLACE FUNCTION process_build_update()
RETURNS trigger AS $$
BEGIN
  IF NEW.buildstatus = 0 AND OLD.buildstatus is NULL THEN
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.drvpath)
        ON CONFLICT (drvpath) DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER build_update_trigger
AFTER UPDATE ON Builds
FOR EACH ROW
WHEN (NEW.buildstatus = 0 AND OLD.buildstatus is NULL)
EXECUTE PROCEDURE process_build_update();

CREATE OR REPLACE FUNCTION process_build_output()
RETURNS trigger AS $$
BEGIN
  IF NEW.path is not NULL THEN
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.path)
        ON CONFLICT (drvpath) DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER build_output_trigger
AFTER INSERT OR UPDATE ON BuildOutputs
FOR EACH ROW
WHEN (NEW.path is not NULL)
EXECUTE PROCEDURE process_build_output();
