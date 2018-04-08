* Question 2 : Bass model;

data sales;
input week sales;
datalines;
1	160 
2	390 
3	800 
4	995 
5	1250
6	1630
7	1750
8	2000
9	2250
10	2500
;
run;

data sales_cumulative;
set sales;
by week;
if week = 1  then cum_sum = 0;
cum_sum + lag(sales);
cum_sum_sq = cum_sum ** 2;
run;

proc print
	data = sales_cumulative;
run;

proc reg
	data = sales_cumulative outest = mylib.est;
	model sales = cum_sum cum_sum_sq / stb ;
run;

data estimates;
	set mylib.est;

	a = Intercept;
	b = cum_sum;
	c = cum_sum_sq;

	M = ((-1 * b) - sqrt(b*b - 4 * a * c))/(2 * c);
	p = a / M;
	q = p + b;

	peak_time = log(q / p) * 1 / (p + q);
	peak_sales = M * ((p + q) ** 2) / (4 * q);

	call symput('p_coeff', p);
	call symput('q_coeff', q);
	call symput('M_coeff', M);

run;

proc print data = estimates; run;

data predicted_sales;
	set sales_cumulative;
	if _N_ = 1 then predicted_sales = symget('p_coeff') * symget('M_coeff');
	if _N_ = 1 then nt1 = 0; 

  	if _N_ > 1 then nt1 = nt1 + predicted_sales;
  	if _N_ > 1 then predicted_sales = ((symget('p_coeff') +(symget('q_coeff') / symget('M_coeff')) * nt1) * (symget('M_coeff') - nt1));
	
	retain nt1;
	retain predicted_sales;
run;

proc print; run;

proc sgplot 
 data = predicted_sales;
 series X = week Y = sales /  legendlabel = 'Actual Sales' markers;
 series X = week Y = predicted_sales /  legendlabel = 'Actual Sales' markers;
 title 'Actual v/s Predicted Sales';
 yaxis label = 'Sales in units';
 xaxis type = DISCRETE;
run; 
