import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { buildMode, nodeEnv, projectPath } from "./paths.mjs";

process.env.NODE_ENV = nodeEnv;

const [{ default: esbuild }, { default: plugin }] = await Promise.all([
  import("esbuild"),
  import("server-reason-react-esbuild-plugin"),
]);

const configPath = fileURLToPath(import.meta.url);
const runningFromBuildDir = configPath.includes("/_build/default/");
const isProduction = buildMode === "production";

// Derive all paths from the project path
const depth = projectPath ? projectPath.split("/").length : 0;
const utopiaDir = projectPath ? `${projectPath}/_utopia` : "_utopia";
const upSegments = (n) => Array(n).fill("..").join("/");

const nodeModules = runningFromBuildDir
  ? `${upSegments(depth + 2)}/node_modules`
  : depth === 0
    ? "./node_modules"
    : `${upSegments(depth)}/node_modules`;

const sourceBuildRoot = depth === 0
  ? "./_build/default"
  : `${upSegments(depth)}/_build/default`;

const melangeTarget = runningFromBuildDir
  ? `_utopia/target/${utopiaDir}`
  : `${sourceBuildRoot}/${utopiaDir}/target/${utopiaDir}`;

const clientEntryPath = `${melangeTarget}/client_entry_melange.js`;
const devClientPath = `${melangeTarget}/Utopia_dev_client.js`;

// Always write to the source tree's _utopia/dist.  When dune runs esbuild
// from _build/default/<project>, the output must NOT land inside the build
// directory — dune cleans up undeclared files there, deleting dist/ when
// other targets in the same subdir are rebuilt.
const sourceProjectRoot = runningFromBuildDir
  ? (projectPath ? `${upSegments(depth + 2)}/${projectPath}` : upSegments(2))
  : ".";
const outdir = `${sourceProjectRoot}/_utopia/dist`;

const bootstrapOutput = `${outdir}/bootstrap.js`;
const browserTargetPrefix = `/target/${utopiaDir}`;
const absoluteTargetPrefix = path.join(process.cwd(), melangeTarget).replaceAll("\\", "/");

// In dev mode, include the dev client as a separate entry point
const devClientExists = !isProduction
  ? await fs.access(devClientPath).then(() => true).catch(() => false)
  : false;

const entryPoints = [
  clientEntryPath,
  ...(devClientExists ? [devClientPath] : []),
];

await esbuild.build({
  entryPoints,
  bundle: true,
  platform: "browser",
  format: "esm",
  splitting: true,
  minify: isProduction,
  outdir,
  define: {
    "process.env.NODE_ENV": JSON.stringify(nodeEnv),
  },
  nodePaths: [nodeModules],
  plugins: [
    plugin({
      target: melangeTarget,
      bootstrapOutput,
      entrypoints: [clientEntryPath],
    }),
  ],
});

const bootstrapContents = await fs.readFile(bootstrapOutput, "utf8");
const rewrittenBootstrap = bootstrapContents
  .replaceAll(absoluteTargetPrefix, browserTargetPrefix)
  .replaceAll(melangeTarget, browserTargetPrefix);

if (rewrittenBootstrap !== bootstrapContents) {
  await fs.writeFile(bootstrapOutput, rewrittenBootstrap, "utf8");
}
