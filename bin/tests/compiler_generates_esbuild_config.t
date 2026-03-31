  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ grep -E 'node:fs/promises|node:path|runningFromBuildDir = configPath.includes|const melangeTarget = runningFromBuildDir|const browserTargetPrefix = "/target/_utopia"|const absoluteTargetPrefix = path.join|entryPoints: \[clientEntryPath\]|target: melangeTarget|bootstrapContents = await fs.readFile|replaceAll\(absoluteTargetPrefix|await fs\.writeFile' _utopia/esbuild.config.mjs
  import fs from "node:fs/promises";
  import path from "node:path";
  const runningFromBuildDir = configPath.includes("/_build/default/");
  const melangeTarget = runningFromBuildDir
  const browserTargetPrefix = "/target/_utopia";
  const absoluteTargetPrefix = path.join(process.cwd(), melangeTarget).replaceAll("\\", "/");
    entryPoints: [clientEntryPath],
        target: melangeTarget,
  const bootstrapContents = await fs.readFile(bootstrapOutput, "utf8");
    .replaceAll(absoluteTargetPrefix, browserTargetPrefix)
    await fs.writeFile(bootstrapOutput, rewrittenBootstrap, "utf8");
