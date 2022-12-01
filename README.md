# web-scalpel

A web scraper tool to gather information about the current market price for electronics in Vietnam. Data taken from popular websites like cellphones.com.vn, thegioididong.com, fptshop.com.vn.

# /backend

There are 2 parts:
- A parser for the data from the HTML downloaded
- A server for a simple API for the front end to query
- Data is saved in a simple SQLite file

Configuration for more links and sites using `config.yaml` file.

# /frontend-svelte

Serve a grid layout and search feature for all the items from the database.
From the grid, user can select multiple items into a list to compare the price and potential source to buy.

# Author

Quang Tran <chop@chop.ink>
