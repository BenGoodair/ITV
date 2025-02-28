**Repo**

Welcome to a repo for data provided to the ITV for outsourcing, quality and expenditure on social care in England.

The rough structure is that we:

a) Have data of all CQC inspections ever pulled from the CQC API (see code in code folder, and underlying data in underlying data folder)

b) Have expenditure data harmonised from the ASC-FR which we have a fully reproducible repo available to see this harmonising process here: [https://github.com/BenGoodair/adults_social_care_data](https://github.com/BenGoodair/adults_social_care_data)

c) Produced some cleaning code to produce the outputs requested (see in code folder).

d) Saved our outputs in the data outputs folder.

There are a bunch of notes and health warnings with this data as posted below.

**Notes for expenditure**

It might well be better for you just to google overall spend - our data was created for the purpose of breaking expenditure down for each LA, rather than a clean number for each year for the whole country.
There just might be slight inconsistencies and errors in our data given the way we processed it.

E.g.  I think Direct payments is just missing one year in the 2000s

These are raw unadjusted figures - you will need to deflate them according to current prices to compare them over time 🙂 

Sadly, before and after 2015, the collection of LA expenditure data changes which has made it incomparable in ways I haven't found possible to reconcile.

**Some of this repo has reprdocubile code, some is a little funky pls reach out to me at benjamin.goodair@bsg.ox.ac.uk for help xx**

**Notes for Inspections**

Care homes aren't inspected every year - so if a care home was inspected "Good" in 2015 and "Outstanding in 2019", it will be recorded in the data as good in 2015, 2016, 2017, 2018. And outstanding 2019, 2020, 2021, 2022...

So this is a snapshot of all care homes each year as opposed to the homes inspected in each year

"Community based adult social care services" are categorised as such according to what locations are inspected for.
