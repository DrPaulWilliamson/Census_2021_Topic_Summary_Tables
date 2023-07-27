# Census 2021 Topic Summary data

Census Topic Summary tables range from the simple (a univariate table providing a set of counts for a discrete set of categories) through to the complex (a table, which might be uni- or multi-variate, providing cell counts for a discrete set of categories, including sub-totals for nested sets of these discrete categories).

**Simple**
<table>
<tbody>
<tr>
<td>All people</td>
<td>100</td>
</tr>
<tr>
<td>Female</td>
<td>55</td>
</tr>
<tr>
<td>Male</td>
<td>45</td>
</tr>
</tbody>
</table>



**Complex**
<table>
<thead>
<tr>
<th>Tier 1</th>
<th>Tier 2</th>
<th>Tier 3</th>
<th>Tier 4</th>
<th>Cell count / <BR>Sub-total</th>
</tr>
</thead>
<tbody>
<tr>
<td>Europe</td>
<td> </td>
<td> </td>
<td> </td>
<td>300</td>
</tr>
<tr>
<td> </td>
<td>United Kingdom</td>
<td> </td>
<td> </td>
<td>50</td>
</tr>
<tr>
<td> </td>
<td>Ireland</td>
<td> </td>
<td> </td>
<td>5</td>
</tr>
<tr>
<td> </td>
<td>Other Europe</td>
<td> </td>
<td> </td>
<td>250</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>EU Member countries</td>
<td> </td>
<td>160</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
<td>France</td>
<td>50</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
<td>Germany</td>
<td>60</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
<td>Italy</td>
<td>50</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Rest of Europe</td>
<td> </td>
<td>90</td>
</tr>
<tr>
<td>Africa</td>
<td> </td>
<td></td>
<td> </td>
<td>100</td>
</tr>
<tr>
<td> </td>
<td>North Africa</td>
<td> </td>
<td> </td>
<td>30</td>
</tr>
</tbody>
</table>

etc.

For complex tables, the challenge is to extract all cell counts that contribute to the table total for a given level of table detail. For example, to extract the 'Tier 4' version of the table above you would need to extract not only the labelled Tier 4 counts (France, Germany, Italy), but also all counts for higher level tiers that contribute to the table total (UK, Ireland, Other Europe, North Africa), whilst omitting the sub-totals that include any of these counts (Europe, EU Member countries, Africa).

The repository folder ["/Data"](/Data) contains the following three datasets:

* Metadata summarising key information about each TS table, including population base, geographies for which the table is available, number of variables, tiers, interior and marginal cell counts, plus the associated England & Wales table total
* Metadata / data dictionary, summarising key information about each TS table cell, including the original ONS cell labels edited to make them more consistent, a unique cell ID (derived from the the ONS table identification code and the sequential ordering of cells within that table), plus the associated England & Wales cell value as a count and percentage
* A file mapping which cells contribute to the table table total for a given tier in the table hierarchy

The repository folder ["/Code"](/Code) includes the code files required to:

* Download all England and Wales 2021 Census Topic Summary tables from [Nomis](https://www.nomisweb.co.uk/sources/census_2021_bulk), unzipping these data, and saving locally as **.csv** files
* Create the TS Table Metadata file
* Create the TS Cell Metadata file, 
* Create the TS Cell Mapping file
* Use the TS Cell Mapping file to extract all of the cell counts that contribute to the table total for a given tier in the table hierarchy
