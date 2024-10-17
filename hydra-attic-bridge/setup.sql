CREATE TABLE IF NOT EXISTS DrvpathsToUpload (
    drvpath TEXT NOT NULL
);

CREATE OR REPLACE FUNCTION process_build_step_update()
RETURNS trigger AS $$
BEGIN
  -- Check if status has been updated to 0
  IF NEW.status = 0 AND OLD.status is NULL THEN
      -- Insert the drvpath into the queue table
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.drvpath);
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
  -- Check if status has been updated to 0
  IF NEW.path is not NULL THEN
      -- Insert the (drv)path into the queue table
      INSERT INTO DrvpathsToUpload (drvpath) VALUES (NEW.path);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER build_step_output_trigger
AFTER INSERT OR UPDATE ON BuildStepOutputs
FOR EACH ROW
WHEN (NEW.path is not NULL)
EXECUTE PROCEDURE process_build_step_output();
