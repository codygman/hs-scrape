# hs-scrape: Web scraping and Automation Library [![Build Status](https://secure.travis-ci.org/codygman/hs-scrape.png)](http://travis-ci.org/codygman/hs-scrape)

# About
A simple and easy scraping automation library in Haskell.

# Examples
[Logging in to Paypal](https://github.com/codygman/hs-scrape-paypal-login)

# Tutorials
Coming soon.

# Credits
Inspired heavily by Shpider, relying extensively on wreq, xml-conduit, and html-conduit. Also inspired by Mechanize from perl (as well as bindings in other languages such as python and ruby).

# TODO
- Make sure dependency constraints are sane/used
- Create tutorials
- Checking reddit messages example
- Logging Into Hacker news example
- Turn example(s?) into tutorials
- Modularize code. It's all in a single file ATM
- getCurrentCursor shouldn't return Maybe, the library should handle it... it's a library error!
- Better test coverage.
