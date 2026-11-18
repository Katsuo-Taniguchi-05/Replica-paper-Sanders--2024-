##############################################################################################
### CODE ARCHIVE FOR 
###	"Dynamic Pricing and Organic Waste Bans: A Study of Grocery Retailers' Incentives to Reduce Food Waste"
###	by
###	Robert Evan Sanders
###
##############################################################################################

Run the following scripts in the following order to generate the structural analysis
for a given synthetic store--product. Depending on the parameters, it should take 
about 15-20 minutes to run the code for a store--product. 

1. synthetic data generator in /code/generate_synthetic_data.R. 
- generates binomial choice panel data
- variables: 
	* store: unique store identifier. (Do one at a time, e.g., store = 1). 
	* date: a numerical representation of a unique day, dictates weekday (e.g., 1-500) 
	* productN: tracks inside vs. outside option, numbered (e.g. 1, 2, 1, 2)
	* price: price in dollars of the inside good and outside good (outside always zero). 
	* productc: tracks inside vs. outside option, name ("Bread 1", "Outside", "Bread 1", ...)	
	* weekday: day of the week (e.g., Monday, Tuesday)
	* daypart: part of the day corresponding to time of day (e.g., 1, 2, ...). There must be K>=2 dayparts.  
	* TripNumber: unique purchase occassion (e.g., 1,1,2,2,3,3,..)
	* dp1...dptK: dummy variable for daypart 
	* choice: name of choice (e.g., "Bread 1", "Outside")
- output:
	* /output/synthetic_data.rds, the choice panel. 
	* /output/synthetic_data_days.rds, maps date to day of the week. 


2. choice model estimation in /code/demand_estimate.R.
- estimate the choice model in R. 
- output:
	* /output/demand_estimates_before_merge.RData, the preference parameter posterior estimates. 
	* /output/demand_estimates_raw_objects.RData, misc. useful objects from the demand estimation. 


3. arrival model estimation in /code/arrivals_estimate.R.
- estimate the arrival model in R. 
- output:
	* /output/arrival_estimates_before_merge.rds, the arrival parameter posterior estimates. 



4. merge together the arrival and choice estimates in /code/combine_demand_estimates.R.
- estimate the arrival model in R. 
- output:
	* /output/masterprefs.rds, arrival and preference parameters merged in a format that can be used by the dynamic program (next script). 


5. solve for the optimal dynamic pricing--inventory policy and the optimal static pricing--inventory policy in /code/supply_side_estimate.R. In addition, saves many key outputs such as the posterior distribution of profit, consumer surplus, and waste under each regime. Key variables to input:
	* mc: marginal cost of producing a new unit of inventory 
	* gammaDxxx = disposal cost per unit of waste, make this higher to simulate an organic waste ban. 
	* checks = (T,F), indicates whether you want to check the policy function satisfies the ACOE
	* drawN: the number of posterior draws you want to calculate outcomes at. Recommended is 50 for computation time, lower this for a lot more speed. 
	* Tp0: shelf life of product, = 2 in the paper. Can be 2 or 3 in the code, beyond this is too resource intensive. 
	* drawdown0: 'FIFO' or 'LIFO', describes how inventory leaves the shelf when consumer buy it. 
	* planningperiods0: number of weeks we solve the finite horizon policy over to guess the infinite horizon solution.  Recommended is 2. 
	* wmax0: number of days in week, always set to 7. 
- output:
	* /output/DP_Result.RData, the outcomes of interest, including the optimal policies under static and dynamic pricing, plus the posterior dist of profits, CS, and waste. 


