  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ test -f _utopia/paths.mjs
  $ cat _utopia/paths.mjs
  export const projectPath = "";
  $ grep -E 'import \{ projectPath \} from|node:fs/promises|node:path|runningFromBuildDir = configPath.includes|const melangeTarget = runningFromBuildDir|const absoluteTargetPrefix = path.join|entryPoints: \[clientEntryPath\]|target: melangeTarget|bootstrapContents = await fs.readFile|replaceAll\(absoluteTargetPrefix|await fs\.writeFile' _utopia/esbuild.config.mjs
  import fs from "node:fs/promises";
  import path from "node:path";
  import { projectPath } from "./paths.mjs";
  const runningFromBuildDir = configPath.includes("/_build/default/");
  const melangeTarget = runningFromBuildDir
  const absoluteTargetPrefix = path.join(process.cwd(), melangeTarget).replaceAll("\\", "/");
    entryPoints: [clientEntryPath],
        target: melangeTarget,
  const bootstrapContents = await fs.readFile(bootstrapOutput, "utf8");
    .replaceAll(absoluteTargetPrefix, browserTargetPrefix)
    await fs.writeFile(bootstrapOutput, rewrittenBootstrap, "utf8");
