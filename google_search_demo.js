const { chromium } = require('playwright');

(async () => {
  console.log('启动浏览器...');
  const browser = await chromium.launch({ 
    headless: false,  // 有头模式，显示浏览器窗口
    slowMo: 1000,     // 减慢操作速度以便观看
    executablePath: 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe', // 使用系统Chrome
    channel: 'chrome'  // 使用Chrome通道
  });
  
  const page = await browser.newPage();
  
  console.log('导航到谷歌...');
  await page.goto('https://www.google.com');
  
  // 等待页面加载
  await page.waitForLoadState('networkidle');
  
  console.log('查找搜索框...');
  // 尝试多个可能的搜索框选择器
  const searchSelectors = [
    'input[name="q"]',
    'textarea[name="q"]', 
    '[data-ved] input',
    'input[type="text"]'
  ];
  
  let searchBox = null;
  for (const selector of searchSelectors) {
    try {
      searchBox = await page.waitForSelector(selector, { timeout: 5000 });
      console.log(`找到搜索框: ${selector}`);
      break;
    } catch (e) {
      console.log(`未找到选择器: ${selector}`);
    }
  }
  
  if (!searchBox) {
    console.log('无法找到搜索框，尝试点击页面中心...');
    await page.click('body');
    await page.waitForTimeout(2000);
    searchBox = await page.waitForSelector('input, textarea', { timeout: 5000 });
  }
  
  console.log('输入搜索内容...');
  await searchBox.fill('Playwright MCP 教程');
  
  console.log('按回车搜索...');
  await page.keyboard.press('Enter');
  
  // 等待搜索结果加载
  await page.waitForSelector('#search');
  
  console.log('截图保存搜索结果...');
  await page.screenshot({ path: 'google_search_result.png', fullPage: true });
  
  console.log('搜索演示完成！等待5秒后关闭浏览器...');
  await page.waitForTimeout(5000);
  
  await browser.close();
  console.log('浏览器已关闭。');
})();
