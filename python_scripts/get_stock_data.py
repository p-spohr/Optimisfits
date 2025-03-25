# Download stock data

# %%
import yfinance as yf
import json

# %%

dax_price = yf.Ticker('DAX.DE')
dax_price.get_info()
dax_price.history(
    start='2017-01-01',
    end='2025-03-21',
    period='1d').to_csv('dax_etf.csv')

# %%
with open('dax_etf.csv', 'w') as fp:
    fp.write(dax_price.history(
        start='2017-01-01',
        end='2025-03-21'))

# %%

# %%

# %%

current_portfolio_tickers = ['MBG', 'BMW', 'EXS1', 'SXR8', 'EUNL', 'SXRI', 'IBCI']

# %%

for ticker in current_portfolio_tickers:
    stock_ticker = yf.Ticker(f'{ticker}.DE')
    stock_info = stock_ticker.get_info()
    with open(f'stock-info/{ticker}.json', mode='w') as file:
        json.dump(stock_info, file)

# %%

for ticker in current_portfolio_tickers:
    stock_ticker = yf.Ticker(f'{ticker}.DE')
    stock_info = stock_ticker.get_info()
    with open(f'stock-info/{ticker}.json', mode='w') as file:
        json.dump(stock_info, file)

# %%
stock_test = yf.Ticker('SXR8.DE')
# %%
stock_test.history(period='1mo')
# %%
shares = stock_test.get_shares_full()
# %%
print(shares)
# %%

from yfinance import EquityQuery
# %%

aggressive_small_caps = EquityQuery('and', [
    EquityQuery('is-in', ['exchange', 'NMS', 'NYQ']),
    EquityQuery('lt', ["epsgrowth.lasttwelvemonths", 15])
])
# %%
response = yf.screen(aggressive_small_caps, sortField = 'percentchange', sortAsc = True)


# %%
type(response)
# %%
import inspect
inspect.getmembers(response)
# %%
print(response)
# %%
response.keys()
# %%
import json
with open('test.json', 'w') as file:
    response_json = json.dump(response, file)
# %%

len(response['quotes'])
# %%

for name in response['quotes']:
    print(name['longName'])

# %%

ger_stocks = EquityQuery('and', [
    EquityQuery('is-in', ['exchange', 'GER']),
    EquityQuery('is-in', ['sector', 'Financial Services'])
])
# %%

ger_response = yf.screen(ger_stocks)
# %%
ger_response.keys()
# %%
ger_response['count']
# %%

for name in ger_response['quotes']:
    print(name['longName'])
# %%

ger_all_stocks = EquityQuery('is-in', ['exchange', 'GER'])

# %%

ger_all_response = yf.screen(ger_all_stocks, size=100)
# %%
ger_all_response['quotes']
# %%
for name in ger_all_response['quotes']:
    try:
        print(name['longName'])
    except:
        print(name['shortName'])

# %%
ger_all_response['count']
# %%
