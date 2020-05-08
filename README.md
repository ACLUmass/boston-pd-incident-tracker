# boston-pd-incident-tracker
ACLUM web application to track Boston Police incidents

Production site: https://data.aclum.org/bpd-incidents/

## Data workflow
<table width="300">
  <tr>
    <th> Virtual Private Server </th>
    <th> shinyapps.io Server </th>
    <th> Virtual Private Server </th>
  </tr>
  <tr>
    <td> Daily cron jobs query the Boston Police Department to obtain latest data, and the updated database is uploaded to AWS... </td>
    <td> ...R Shiny app grabs data from AWS and exposes app at laurenmarietta.shinyapps.io domain...</td>
    <td> ...Hosts HTML file with iframe embedding shiny app, with desired ACLU URL & favicon </td>
  </tr>
</table>
