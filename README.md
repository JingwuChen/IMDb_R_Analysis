# IMDb web scrapping and data analysis
## 本文采用rvest库爬取IMDb，然后采用ggplot2以及一些必要的统计方法进行数据分析，最后用shiny库进行网页显示
本文是承接上一篇[shiny_ggplot2 repository](https://github.com/JingwuChen/shiny_ggplot2)而来的，里面有关于rvest的使用方法和ggplot在shiny中应用示例，想进一步了解的可以去看看:smirk::smirk::smirk:

当然这篇也参考了国外一篇rvest IMDb的佳作，[详见](https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/)

在这里只强调一点关于爬虫的经验，就是如果爬取类似电影(或商品)卡片型的网站,最好就将每个卡片作为一个section，再从每个section里面单独爬取各标签元素内容，这样能够避免某个电影卡片缺失某个元素造成后一个卡片的标签元素上前补位造成爬取的某个变量的所有值错位了。

简单使用`html_nodes()`全部爬取必然造成票房数据错位了，上面的国外那篇博文中采取是手动后期改造，其实按照我上面说法，先怕每个section，然后采用`html_node()`就能爬取到所有元素，缺失某个元素以NA代替了。

