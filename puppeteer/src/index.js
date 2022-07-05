const puppeteer = require('puppeteer');
const fs = require('fs');
const yaml = require('js-yaml');

const PROCESSING_CHUNK_SIZE = 3;
const BROWSER_TIMEOUT = 20000;

const argv = require('yargs/yargs')(process.argv.slice(2))
  .usage('Usage: $0 -c [config] -d [data_folder]')
  .demandOption(['c', 'd']).argv;

const getDateStr = () => new Date().toJSON().slice(0, 10);
const outputDir = argv.d + getDateStr();
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
      const browser = await puppeteer.launch();

      // Process the list by link and download name
      const tasks = processList
        .sort(() => Math.random() - 0.5) // Random to not query one website too much
        .map(async ({link, name}, index) => {
          // Not spamming the server by delay abit
          const waitTimeByChunk = index % PROCESSING_CHUNK_SIZE;
          await new Promise(resolve =>
            setTimeout(resolve, waitTimeByChunk * 1000)
          );

          console.log('start >> ', link, name, waitTimeByChunk);

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
          fs.writeFileSync(outputDir + '/' + name + '.html', html);
          await bodyHandle.dispose();

          console.log('done >> ', link, name);
        });

      // Do all tasks async in chunk of tasks
      for (const c of chunk(tasks, PROCESSING_CHUNK_SIZE)) {
        await Promise.all(c);
      }

      await browser.close();
    } catch (e) {
      console.log(e);
    }
  } catch (e) {
    console.log(e);
  }
})();
