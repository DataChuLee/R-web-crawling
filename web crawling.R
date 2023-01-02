# 웹 크롤링 하는 방식
# Get / html / Post / JS * 로그인 (유, 무) -> 로그인 (유) 으로 하면 어렵다. (무) 로 하는 것을 추천한다. 
# JS * Get 방식 * 로그인(무) 방식이 쉽다. 
# Get --> query /Post --> body : text , payload : JSON  
# 크롤링 막무가내로 하면 안된다!! 
# https://github.com/HelloDataScience/R4WC
# 웹 크롤링이 가능한 사이트를 확인할려면 페이지 뒤에 /robots.txt을 붙여서 검색! 

library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(magrittr)

## GET 방식의 웹 크롤링 
url <- "https://www.diningcode.com/list.php"
query <- list(query = "초밥")
res <- GET(url = url, query = query)
print(x = res)



res %>% content(as = 'text')
 "#div_list > li:nth-child(4) > a > span.btxt" # . : class 를 뜻함 / 부등호 방향 : 자식 부모 관계를 뜻함/ # : id를 뜻한다.

# 한 가게의 이름만 갖고 오고 싶을 떄
 res %>% 
  read_html() %>% 
  html_node(css ="#div_list > li:nth-child(4) > a > span.btxt") %>%
  html_text()

items <- res %>% 
  read_html() %>% 
  html_nodes(css = "#div_list > li > a") # html_node() --> 1개의 정보를 갖고 올때 / html_nodes() : 여러 개 정보를 갖고 싶을 때
  
# 아래의 가게 정보를 함수로 만들어서 코드 단축화 
getText <-function(html,css){
  html %>% 
    html_node(css = css) %>% 
    html_text(trim = T) %>% 
    return()
}
getText(html = items, css = 'span.btxt')
getText(html = items, css = 'span.stxt')
getText(html = items, css = 'span.ctxt')
getText(html = items, css = 'span:nth-child(5)')


df <- data.frame(
  name = getText(html = items, css = 'span.btxt'),
  menu = getText(html = items, css = 'span.stxt'),
  text = getText(html = items, css = 'span.ctxt'),
  addr = getText(html = items, css = 'span:nth-child(5)')
)
View(df)


# TOP 10 가게의 이름 
items %>% 
  html_node(css = 'span.btxt') %>% 
  html_text(trim = T) # trim = T --> 문자형 벡터를 출력할 때 양 옆에 존재하는 공백을 지워주는 기능을 한다. 

# TOP 10 가게의 대표메뉴 
items %>% 
  html_node(css = 'span.stxt') %>% 
  html_text(trim = T)

## POST 방식의 웹 크롤링 
url <- "https://www.diningcode.com/2018/ajax/list.php"
body <- list(
  query: '초밥',
  page: 2, # 만약 더 많은 가게들의 정보를 보고 싶으면 page 인자의 숫자를 바꾸면 된다.
  chunk: 10
) # GET 방식에서의 query 대신 POST 에서는 body 를 사용한다. 

res <- POST(url = url, body = body, encode = 'form')

items <- res %>% 
  read_html() %>% 
  html_nodes(css = "a") # POST 방식은 Headers가 아닌 Response에서 봐야한다.

df1 <- data.frame(
  name = getText(html = items, css = 'span.btxt'),
  menu = getText(html = items, css = 'span.stxt'),
  text = getText(html = items, css = 'span.ctxt'),
  addr = getText(html = items, css = 'span:nth-child(5)')
)
df1
View(df1)

df <- rbind(df, df1)

## POST 방식을 통한 네이버 카페 크롤링 -> 검색창에 search가 기입되어있으면 ip주소가 짤리니 주의해라 (3일정도 크롤링 못한다)
# 1. 카페의 제목 링크를 먼저 수집
# 2. 그 링크안에 수집하고자 하는 텍스트를 모은다. blog도 마찬가지! 

url <- "https://apis.naver.com/cafe-home-web/cafe-home/v1/search/articles"
# payload에 위치한 정보
body <- list(
  exceptMarketArticle=1,
  page=1,
  period= c("20221225", "20221231"),
  query="오징어게임 ",
  sortBy=0
)
# 그 정보를 JSON형태로 바꿔야한다. 그래서 toJOSN 이라니 함수를 사용함. 그전에 library(jsonlite)를 해야한다. 
body %<>% toJSON(auto_unbox = TRUE)
body

# 이전의 다이닝코드 사이트에서의 POST방식은 url,body 뿐이였는데, 카페 or 블로그에서의 POST 방식에서는 
# add_headers(referer = 'https://section.cafe.naver.com/ca-fe/home/search/articles',
# `content-type` = 'application/json;charset=UTF-8')와 user_agent(agent = 'Mozilla/5.0 Chrome/108.0.0.0 Safari/537.36')를 추가해야한다. 
res <- POST(
  url = url,
  body = body,
  add_headers(referer = 'https://section.cafe.naver.com/ca-fe/home/search/articles',
              `content-type` = 'application/json;charset=UTF-8'), # 변수명에 하이퍼링크(-)가 들어가서 ``를 기입입한 것!
  user_agent(agent = 'Mozilla/5.0 Chrome/108.0.0.0 Safari/537.36')
)
print(x = res)
res %>% content(as = 'text') # 출력에 500이라는 인터넷 서버 에러가 나타나는데, 이것은 네이버에서 너 사람이냐? 라고 묻는 거이므로, 다시 POST 함수에서 내가 사람인것을 밝히면 된다.

data <- res %>% 
  content(as = 'text') %>% 
  fromJSON()

class(data)
names(x = data$message$result$searchResultArticle$searchResult)
df <- x = data$message$result$searchResultArticle$searchResult

