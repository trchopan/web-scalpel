const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');

const BROWSER_TIMEOUT = 20000;

const argv = require('yargs/yargs')(process.argv.slice(2))
  .usage('Usage: $0 -c [config] -d [data_folder]')
  .demandOption(['c', 'd']).argv;

const getDateStr = () => new Date().toJSON().slice(0, 10);
const outputDir = path.join(argv.d, getDateStr());
const existFolder = fs.existsSync(outputDir);
if (!existFolder) {
  fs.mkdirSync(outputDir);
}

const chunk = (arr, chunkSize) => {
  const res = [];
  for (let i = 0; i < arr.length; i += chunkSize) {
    const chunk = arr.slice(i, i + chunkSize);
    res.push(chunk);
  }
  return res;
};

(async () => {
  try {
    const fileContents = fs.readFileSync(argv.c, 'utf8');
    const processList = yaml.load(fileContents);

    try {
      const launchOption = process.env.CHROMIUM_PATH ? {executablePath: process.env.CHROMIUM_PATH} : undefined;
      const browser = await puppeteer.launch(launchOption);

      // Process the list by link and download name
      const tasks = processList
        .sort(() => Math.random() - 0.5) // Random to not query one website too much
        .map(({link, name}) => async () => {
          // Not spamming the server by delay abit
          console.log('start >> ', link, name);

          const page = await browser.newPage();
          await page.goto(link, {
            waitUntil: 'networkidle2',
            timeout: BROWSER_TIMEOUT,
          });
          const bodyHandle = await page.$('body');
          const bodyBoundingBox = await bodyHandle.boundingBox();
          await page.mouse.wheel({deltaY: bodyBoundingBox.height});
          await page.waitForTimeout(3 * 1000);
          const html = await page.evaluate(body => body.innerHTML, bodyHandle);
          fs.writeFileSync(path.join(outputDir, name + '.html'), html);
          await bodyHandle.dispose();
          await page.close();

          console.log('done >> ', link, name);
        });

      for (const task of tasks) {
        try {
          await task();
        } catch (err) {
          console.log('Failed chunk >> retry after 5s');
          await new Promise(resolve => setTimeout(resolve, 5000));
          await task();
        }
      }

      await browser.close();
    } catch (e) {
      console.log(e);
    }
  } catch (e) {
    console.log(e);
  }
})();
