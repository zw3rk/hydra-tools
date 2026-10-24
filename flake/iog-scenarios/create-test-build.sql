-- Forge a fake build in the Hydra database. We're assuming the project and jobset
-- already exist at input-output-hk-sample/pullrequest-1347.

-- Forge a successful evaluation
INSERT INTO jobsetevals 
  ( jobset_id,
    timestamp, 
    checkouttime, 
    evaltime, 
    hasnewbuilds, 
    hash, 
    flake
  )
SELECT 
  id,
  extract(epoch from now())::int,
  1,
  1,
  1,
  '0000000000000000000000000000000000000000000000000000000000000000',
  'github:input-output-hk/sample/0000000000000000000000000000000000000000'
FROM jobsets WHERE project = 'input-output-hk-sample' AND name = 'pullrequest-1347'
RETURNING id \gset eval_

-- Forge a failed build
INSERT INTO builds 
  ( finished,
    timestamp, 
    jobset_id, 
    job, 
    drvpath, 
    system, 
    starttime, 
    stoptime, 
    buildstatus
  )
SELECT 
  1,
  extract(epoch from now())::int,
  id, 
  :'job_name',
  :'drv_path',
  'x86_64-linux',
  extract(epoch from now())::int,
  extract(epoch from now())::int,
  1
FROM jobsets WHERE project = 'input-output-hk-sample' AND name = 'pullrequest-1347'
RETURNING id \gset build_

-- Forge a build step.
INSERT INTO buildsteps 
  ( build,
    stepnr, 
    type, 
    drvpath, 
    busy, 
    status, 
    starttime, 
    stoptime
  )
VALUES
  ( :build_id,
    1,
    0,
    :'drv_path',
    0,
    1,
    extract(epoch from now())::int,
    extract(epoch from now())::int
  );

-- Add the forged build to an existing eval.
INSERT INTO jobsetevalmembers (eval, build, isnew)
VALUES (:eval_id, :build_id, 1)
RETURNING build;
