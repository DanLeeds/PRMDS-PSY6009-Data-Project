PEW RESEARCH CENTER
Wave 42 American Trends Panel 
Dates: January 7-January 21, 2019
Mode: Web

Sample: Subsample
Language: English and Spanish
N=4,464


***************************************************************************************************************************
NOTES


The following variables were created through thematic coding done by researchers on responses to open-ended survey items. For instances in which responses touched 
on more than one of the themes highlighted in the coding scheme, respondents were assigned multiple codes in the order in which each theme was mentioned in their
response. The coded variables below represent the first, second, and third mentions of themes from the coding scheme within individual item responses (as applicable).

SC1POS_W42_OE1
SC1POS_W42_OE2
SC1POS_W42_OE3
SC1NEG_W42_OE1
SC1NEG_W42_OE2
SC1NEG_W42_OE3
FUTURE_BOE_OE1
FUTURE_BOE_OE2
FUTURE_BOE_OE3
FUTURE_WOE_OE1
FUTURE_WOE_OE2
FUTURE_WOE_OE3

The following variables are included in the dataset to identify correct answers to science knowledge questions KNOW1-KNOW12 (note that there is no KNOW8). 
KNOW1_CORRECT_W42
KNOW2_CORRECT_W42
KNOW3_CORRECT_W42
KNOW4_CORRECT_W42
KNOW5_CORRECT_W42
KNOW6_CORRECT_W42
KNOW7_CORRECT_W42
KNOW9_CORRECT_W42
KNOW10_CORRECT_W42
KNOW11_CORRECT_W42
KNOW12_CORRECT_W42
KNOW_INDEX_W42 (see syntax below for how to create)


***************************************************************************************************************************
WEIGHTS 

WEIGHT_W42 is the weight for the sample. Data for all Pew Research Center reports are analyzed using this weight.

***************************************************************************************************************************
Releases from this survey:

March 28, 2019 "What Americans Know About Science"
https://www.pewresearch.org/science/2019/03/28/what-americans-know-about-science/

August 2, 2019 "Trust and Mistrust in Americans’ Views of Scientific Experts"
https://www.pewresearch.org/science/2019/08/02/trust-and-mistrust-in-americans-views-of-scientific-experts/

FactTank posts

March 28, 2020, “Science knowledge varies by race and ethnicity in U.S.”, 
https://www.pewresearch.org/fact-tank/2019/03/28/science-knowledge-varies-by-race-and-ethnicity-in-u-s/

August 5, 2020, “5 key findings about public trust in scientists in the U.S.”, 
https://www.pewresearch.org/fact-tank/2019/08/05/5-key-findings-about-public-trust-in-scientists-in-the-u-s/

August 9, 2020, “Democrats and Republicans differ over role and value of scientists in policy debates”, 
https://www.pewresearch.org/fact-tank/2019/08/09/democrats-and-republicans-role-scientists-policy-debates/

August 19, 2019, “Most Americans have positive image of research scientists, but fewer see them as good communicators”, 
https://www.pewresearch.org/fact-tank/2019/08/19/most-americans-have-positive-image-of-research-scientists-but-fewer-see-them-as-good-communicators/

August 27, 2020, “Most Americans say science has brought benefits to society and expect more to come”, 
https://www.pewresearch.org/fact-tank/2019/08/27/most-americans-say-science-has-brought-benefits-to-society-and-expect-more-to-come/

October 4, 2020, “Most Americans are wary of industry-funded research,” 
https://www.pewresearch.org/fact-tank/2019/10/04/most-americans-are-wary-of-industry-funded-research/


***************************************************************************************************************************
Syntax

KNOW_INDEX_W42 was created using the following SPSS syntax:

compute KNOW_INDEX_W42 = KNOW1_CORRECT_W42 + KNOW2_CORRECT_W42 + KNOW3_CORRECT_W42 + KNOW4_CORRECT_W42 + KNOW5_CORRECT_W42 + KNOW6_CORRECT_W42 + KNOW7_CORRECT_W42 + KNOW9_CORRECT_W42 + KNOW10_CORRECT_W42 + KNOW11_CORRECT_W42 + KNOW12_CORRECT_W42.
variable labels KNOW_INDEX_W42 'Total number of correct science knowledge answers 0 to 11'.

