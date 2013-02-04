<head>
<title>Space Weather Research Lab of NJIT</title>
<style type="text/css">
body {
	background:#ffffff url('img_tree.png') no-repeat right top;
	background-color: #CCCCCC;
}
</style>
<meta http-equiv="Content-Type" content="text/html; charset=gb2312"></head>

<body >
<table  border=1 bordercolor=coral cellspacing="0" cellpadding="2" >
<tr ><td width="100%" valign=top style="background: #CCFFFF;">
<?php
	$today = time();
	$fileDate = filectime("prob.txt");
	if ( ( $today - $fileDate ) < 36*3600 ){
		
?>
<h3>Probability Forecasting of Flares</h3><br/>		
    <table width="100%" border="1" bordercolor="#000000" cellspacing="0" cellpadding="2">
	<tr>
		<th>Date</th><th>Region Number</th><th>Helicity Injection Rate (10^40 Mx^2/hr)</th><th>Total Unsigned Flux (10^20 Mx)</th><th>0-24 hrs flare index</th><th>24-48 hrs flare index</th><th>C-flare 0-24 hrs</th><th>C-flare 24-48 hrs</th><th>M-flare 0-24 hrs</th><th>M-flare 24-48 hrs</th><th>X-flare 0-24 hrs</th><th>X-flare 24-48 hrs</th>
	</tr>
	<?php
	$fn_prob = "daily.txt";
	if(file_exists($fn_prob)){
		$fh_prob = fopen($fn_prob,'r');
		while(!feof($fh_prob)){
		
			$line1 = fgets($fh_prob);
		
			$parts = explode(',',$line1);
			print '<tr align="center"><td>' . $parts[0] . '</td><td>' . $parts[1] . '</td><td>' . $parts[2] . '</td><td>' . $parts[3] . '</td><td>' . $parts[4] . '</td><td>' . $parts[5] . '</td><td>' . $parts[6] . '</td><td>' . $parts[7] . '</td><td>' . $parts[8] . '</td><td>' . $parts[9] . '</td><td>' . $parts[10] . '</td><td>' . $parts[11] . '</td></tr>';
		       
                       
		}
		fclose($fh_prob);
	}
	?>
    </table>

 <p><!--Updated at  
	
    <p>Updated at  
	<?php if (file_exists($fn)) echo gmdate('Y-n-d',filectime("para.txt"))?>T00:00:00+0000 GMT <br/>
	
<? 
}else{
 print 'Today, there is no active regions within the center region of the Solar disk for flares forecasting !'; 
 print '<br/>';
 print 'Or there is no enough SOHO/MDI magnetograms for flare forecasting!';
	
}
?>   
    <!--For more history forecasting results, <a href="http://spreadsheets.google.com/pub?key=tpidfsKNIyIClDP9SW8o0MA&output=html" target="_blank">click here. </a><br/>-->

    For historical forecasting results, <a href="flare_forecasting.csv" target="_blank">click here. </a><br/>
    For information on the forecasting, <a href="/h2f/index.html" target="_blank">click here. </a></p>
 
</td>
<tr>
<td width="585" valign=top style="background: #FFFFCC;">



<h3>Daily Filament Detection</h3>


	<table  width="500" border="1" bordercolor="#000000" cellspacing="0" cellpadding="2">
 <tr><th COLSPAN="7"><h3>Detailed Filaments Measurements</h3></th></tr>
      <tr>
        <th width="10">id </th>
        <th width="60">Area(Square Mega-meters)</th>
        <th width="60">Longitude(degrees)</th>
        <th width="160">Latitude(degrees)</th>
        <!--th width="120">Length(Kilometers) </th-->
	
	<!--th >Slope</th-->
      </tr>
	  <?php
		$fn = "current/result.txt";
		if (file_exists($fn)){
		$fh = fopen($fn,'r');
		while(!feof($fh)){
			$line = fgets($fh);

			$pos = strpos($line,'%');
			if($pos === false) {
	 			// string needle found in haystack
				//continue;
			}else{
				continue;
			}



			$parts = explode(',',$line);
			print '<tr align="center">
			<td>' . $parts[0] .  '</td>
			<td>' . $parts[1] . '</td>
			<td>' . $parts[2] . '</td>
			<td>' . $parts[3] . '</td>
			<!--td>' . $parts[4] . '</td>
			<td>' . $parts[5] . '</td>
			<td>' . $parts[6] . '</td-->
    		</tr>';
		}
		fclose($fh);
	}
 ?>
 
 </tr>
    </table><br/>
	<center><a href="current/char.png" target="_blank"><img src="current/char.png" widht=500 height=500 /></a></center>
<p>Updated at  
	<?php if (file_exists("current/result.txt"))  echo date("Y-m-d") ?>T00:00:00+0000 GMT <br/>


</td></tr>
<!--tr><td>

</td><td>

</td></tr!-->
</table>

<!-- end masthead -->
</body>
</html>
