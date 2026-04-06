  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler --mode production > /dev/null
  $ test -f _utopia/paths.mjs
  $ cat _utopia/paths.mjs
  export const projectPath = "";
  export const buildMode = "production";
  export const nodeEnv = "production";
  $ grep -E 'import \{ buildMode, nodeEnv, projectPath \} from|process\.env\.NODE_ENV = nodeEnv|Promise\.all\(|import\("esbuild"\)|import\("server-reason-react-esbuild-plugin"\)|node:fs/promises|node:path|const isProduction = buildMode === "production"|runningFromBuildDir = configPath.includes|const melangeTarget = runningFromBuildDir|const absoluteTargetPrefix = path.join|minify: isProduction|"process\.env\.NODE_ENV": JSON\.stringify\(nodeEnv\)|entryPoints: \[clientEntryPath\]|target: melangeTarget|bootstrapContents = await fs.readFile|replaceAll\(absoluteTargetPrefix|await fs\.writeFile' _utopia/esbuild.config.mjs
  import fs from "node:fs/promises";
  import path from "node:path";
  import { buildMode, nodeEnv, projectPath } from "./paths.mjs";
  process.env.NODE_ENV = nodeEnv;
  const [{ default: esbuild }, { default: plugin }] = await Promise.all([
    import("esbuild"),
    import("server-reason-react-esbuild-plugin"),
  const runningFromBuildDir = configPath.includes("/_build/default/");
  const isProduction = buildMode === "production";
  const melangeTarget = runningFromBuildDir
  const absoluteTargetPrefix = path.join(process.cwd(), melangeTarget).replaceAll("\\", "/");
    entryPoints: [clientEntryPath],
    minify: isProduction,
      "process.env.NODE_ENV": JSON.stringify(nodeEnv),
        target: melangeTarget,
  const bootstrapContents = await fs.readFile(bootstrapOutput, "utf8");
    .replaceAll(absoluteTargetPrefix, browserTargetPrefix)
    await fs.writeFile(bootstrapOutput, rewrittenBootstrap, "utf8");
