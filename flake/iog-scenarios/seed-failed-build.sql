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
VALUES
  ( ( SELECT id FROM jobsets
      WHERE name = 'pullrequest-1347'
      AND project = 'input-output-hk-sample'
    ),
    extract(epoch from now())::int,
    1,
    1,
    1,
    '0000000000000000000000000000000000000000000000000000000000000000',
    'github:input-output-hk/sample/0000000000000000000000000000000000000000'
  );

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
VALUES
  ( 0,
    extract(epoch from now())::int,
    ( SELECT id FROM jobsets
      WHERE name = 'pullrequest-1347'
      AND project = 'input-output-hk-sample'
    ),
    'required.test',
    '/nix/store/ply5hyh8fxwkp82jsp42h8l8aq6zw7-test.drv',
    'x86_64-linux',
    extract(epoch from now())::int,
    extract(epoch from now())::int,
    9
  );

-- Forge a build step. It's pretty fragile, it assumes there's only one build.
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
  ( (SELECT id FROM builds ORDER BY id DESC LIMIT 1),
    1,
    0,
    '/nix/store/ply5hyh8fxwkp82jsp42h8l8aq6zw7-test.drv',
    0,
    1,
    extract(epoch from now())::int,
    extract(epoch from now())::int
  );


-- Add the forged build to an existing eval. This is fragile, it assumes there's
-- only one eval and build.
INSERT INTO jobsetevalmembers
  (eval, build, isnew)
VALUES
  ( (SELECT id FROM jobsetevals ORDER BY id DESC LIMIT 1),
    (SELECT id FROM builds ORDER BY id DESC LIMIT 1),
    1
  );

-- Forge a build step. This is fragile, it assumes there's only one existing build.
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
  ( (SELECT id FROM builds ORDER BY id DESC LIMIT 1),
    1,
    0,
    '/nix/store/ply5hyh8fxwkp82jsp42h8l8aq6zw7-test.drv',
    0,
    1,
    extract(epoch from now())::int,
    extract(epoch from now())::int
  );

