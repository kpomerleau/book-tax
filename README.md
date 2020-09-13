Under construction! :)

**Methodology for Estimating the Marginal Effective Tax Rate on New Investment Under Biden's Proposed Book Minimum Tax**

The following is the methodology for estimating the marginal effective tax rate under Biden's proposal minimum tax on "book" income. The methodology is adapted from "Understanding Investment Incentives Under Parallel Tax Systems: An Application to the Alternative Minimum Tax" by Andrew B. Lyon (1989).

The method starts with the standard cost of capital formula from Jogensen (1967). The cost of capital is equal to the gross return required to cover tax, depreciation, and the return demanded by shareholders.

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=c=\frac{(r %2B\delta)(1-uz-k)}{(1-u)}"></div>

where <img src="https://render.githubusercontent.com/render/math?math=c"> is the cost of capital, <img src="https://render.githubusercontent.com/render/math?math=r"> is the firm's real discount rate, <img src="https://render.githubusercontent.com/render/math?math=\delta"> is economic depreciation, <img src="https://render.githubusercontent.com/render/math?math=u"> is the statutory tax rate, <img src="https://render.githubusercontent.com/render/math?math=z"> is the present discounted value of depreciation deductions, and <img src="https://render.githubusercontent.com/render/math?math=k"> is any investment tax credit.

This discount rate <img src="https://render.githubusercontent.com/render/math?math=r"> is the weighted average rate for equity and debt-financed investment:

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=r=E(1-f)%2bf(i(1-ub)-\pi)"></div>

<img src="https://render.githubusercontent.com/render/math?math=f"> is the share of investment financed with debt, <img src="https://render.githubusercontent.com/render/math?math=E"> is the real return on equity, <img src="https://render.githubusercontent.com/render/math?math=i"> is the nominal interest rate, <img src="https://render.githubusercontent.com/render/math?math=c">u is the statutory corporate tax rate, <img src="https://render.githubusercontent.com/render/math?math=b"> is the share of debt deductable against corporate taxable income, and <img src="https://render.githubusercontent.com/render/math?math=\pi"> is the inflation rate.

The standard cost of capital formula assumes that tax policy is constant over the life of an asset. It does not consider the impact of rate or base changes the could occur. In order to account for that, the cost of capital formula is expanded to account for different tax rates and deductions in different periods.

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=c=\frac{(r%2B\delta)(1-\sum_{t=0}^nu_tz_tp_t-k_tp_t)}{1-\frac{(\sum_{t=0}^\infty u_t\Pi_tp_t)}{(\sum_{t=0}^\infty \Pi_tp_t)}}"></div>

The expression, <img src="https://render.githubusercontent.com/render/math?math=\sum_{t=0}^nu_tz_tp_t">, is the sum of the value of deductions <img src="https://render.githubusercontent.com/render/math?math=z"> for an investment times the tax rate <img src="https://render.githubusercontent.com/render/math?math=u"> in each period, <img src="https://render.githubusercontent.com/render/math?math=t">, discounted to present value with discount factor, <img src="https://render.githubusercontent.com/render/math?math=p">.

The expression, <img src="https://render.githubusercontent.com/render/math?math=1-\frac{(\sum_{t=0}^\infty u_t\Pi_tp_t)}{(\sum_{t=0}^\infty \Pi_tp_t)}">, is the share of earnings <img src="https://render.githubusercontent.com/render/math?math=\Pi"> taxed at the statutory tax rate <img src="https://render.githubusercontent.com/render/math?math=u"> in each period, <img src="https://render.githubusercontent.com/render/math?math=t">, discounted to present value with the discount factor, <img src="https://render.githubusercontent.com/render/math?math=p">.

A firm that is subject to the parellel tax system generates tax credits for timing differences that offset future ordinary tax liabiliy. There are two components of the credits: <img src="https://render.githubusercontent.com/render/math?math=V"> and <img src="https://render.githubusercontent.com/render/math?math=W">.

<img src="https://render.githubusercontent.com/render/math?math=V"> is equal to the difference between the tax value of deductions under the ordinary tax and the parellel tax system. <img src="https://render.githubusercontent.com/render/math?math=u"> is the statutory tax rate for the ordinary corporate tax, <img src="https://render.githubusercontent.com/render/math?math=m"> is the tax rate under the parallel tax system, and <img src="https://render.githubusercontent.com/render/math?math=z"> is the deduction for that year.

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=v= \sum_{t=p}^nuz_t - \sum_{t=0}^nmz_t"></div>

<img src="https://render.githubusercontent.com/render/math?math=W"> is the difference in the tax a firm pays on earnings under the parellel system and the tax paid on earnings under the ordinary corporate tax. <img src="https://render.githubusercontent.com/render/math?math=u"> is the statutory tax rate for the ordinary corporate tax, <img src="https://render.githubusercontent.com/render/math?math=m"> is the tax rate under the parallel tax system, and <img src="https://render.githubusercontent.com/render/math?math=\Pi"> is the earnings in each year.

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=w= (m - u)(\sum_{t=p}^\n \Pi_t)"></div>

The value of credits <img src="https://render.githubusercontent.com/render/math?math=V">, <img src="https://render.githubusercontent.com/render/math?math=W">, and <img src="https://render.githubusercontent.com/render/math?math=k"> are all discounted with discount factor <img src="https://render.githubusercontent.com/render/math?math=p"> to reflect the years in which firms receive these credits. This analysis assumes that firms fully recover credits the first year in which they can.

The final cost of capital formula is:

<div align="center"><img src="https://render.githubusercontent.com/render/math?math=c=\frac{(r%2B\delta)(1-\sum_{t=0}^nu_tz_tp_t-k_tp_t-Vp_t)}{1-\frac{(\sum_{t=0}^\infty u_t\Pi_tp_t-Wp_t)}{(\sum_{t=0}^\infty \Pi_tp_t)}}"></div>

***Parameters and assumptions***

For the most part, this analysis uses assumptions that are consistent with "The Tax Burden on Business Investment Under Joe Biden's Tax Proposals" https://www.aei.org/research-products/report/the-tax-burden-on-business-investment-under-joe-bidens-tax-proposals/

This analysis assumes that the book tax would be structured like an alternative minimum tax. Corporations under the book tax would depreciation assets in line with economic depreciation.

| Parameter | Value |
|---|---|
|Ordinary corporate tax rate|28%|
|Book tax rate|15%|
|Return on equity 'E'| 6.8%|
|Interest rate 'i'|5.8%|
|Inflation rate|2.4%|
|Share investment debt financed|32%|
|Share of debt deductible (ordinary corporate tax)| 84%|
|Share of debt deductible (book tax)|100%|
|R&D tax credit value| 2.17%|
|Share of inventory using LIFO| 50%|
|Inventory holding period (years)| .33|

***Description of Files in Repository***

| File | Description |
|---|---|
|asset_data.csv and asset_data.xlsx| A file containing corporate capital stock data. It includes information on the size of the capital stock by asset, its major asset group, its ADS and GDS asset life, and its economic depreciation rate from the BEA.|
|script.r|An r script that produces the estimates|
