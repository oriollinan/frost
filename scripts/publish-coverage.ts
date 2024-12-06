/// <reference lib="dom" />

import { $ } from "npm:zx";
import puppeteer from "https://deno.land/x/puppeteer@16.2.0/mod.ts";

const WEBHOOK_URL = Deno.args[0];
const CHROME_PATH = Deno.args[1];

if (!WEBHOOK_URL) {
  throw new Error("WEBHOOK_URL is not set");
}

if (!CHROME_PATH) {
  throw new Error("CHROME_PATH is not set");
}

const hpcRootPath =
  await $`find dist-newstyle -name hpc_index.html | head -n 1`;

const browser = await puppeteer.launch({
  args: [
    "--no-sandbox",
    "--disable-dev-shm-usage",
  ],
  executablePath: CHROME_PATH,
});

const page = await browser.newPage();

await page.goto(`file://${Deno.cwd()}/${hpcRootPath.stdout.trim()}`);

const { width, height } = await page.evaluate(() => {
  const body = document.body;
  const html = document.documentElement;

  const contentWidth = Math.max(
    body.scrollWidth,
    body.offsetWidth,
    html.clientWidth,
    html.scrollWidth,
    html.offsetWidth,
  );
  const contentHeight = Math.max(
    body.scrollHeight,
    body.offsetHeight,
    html.clientHeight,
    html.scrollHeight,
    html.offsetHeight,
  );

  return { width: contentWidth, height: contentHeight };
});

await page.setViewport({ width, height });

await page.screenshot({ path: "coverage.png", type: "png" });

await browser.close();

const form = new FormData();

const gitCommitHash = (await $`git rev-parse HEAD`).stdout.trim();

form.append(
  "payload_json",
  JSON.stringify({
    embeds: [
      {
        title: "HPC Test Report",
        description: `\`\`\`md\n# ${gitCommitHash}\n\`\`\``,
        image: {
          url: "attachment://coverage.png",
        },
        color: 0x36393f,
      },
    ],
  }),
);

form.append(
  "file2",
  new Blob([await Deno.readFile(`./coverage.png`)], { type: "image/png" }),
  "coverage.png",
);

const req = await fetch(WEBHOOK_URL, {
  body: form,
  method: "POST",
});

if (!req.ok) {
  await fetch(WEBHOOK_URL, {
    method: "POST",
    body: JSON.stringify(
      `\`\`\`json\n${JSON.stringify(await req.json(), null, 2)}\n\`\`\``,
    ),
  });
}

await $`rm coverage.png`;
