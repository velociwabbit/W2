<?php
header("Content-type: application/json"); 
$data1	= $_POST["str"]	 ;	
if (empty($data1) ) $data = 	  $_GET["str"]	;
$conn	= sqlsrv_connect("71.193.245.92",array("UID"=>"sa"	, "PWD"=>"steve_4545"	, "Database"=>"DrugTestingManagement"));	
$stmt	= sqlsrv_query($conn,	sprintf("EXECUTE [DrugTestingManagement].[dbo].[SP_Process_JSON_UPDATE_JPR] @JSONSTUFF=N' $data '" )	);
$next	= sqlsrv_next_result($stmt);		
$debg	= sqlsrv_fetch_array($stmt, SQLSRV_FETCH_ASSOC);
$jcod	= json_encode($debg);
 $f		= fopen("debug.js","w");   fprintf($f,"%s\n%s\n%s\n%s\n" ,   $data,$next, $debg, $jcod);  fclose($f);
echo $jcod;
$from	  =  "info@DrugTestingManagment.com" 
$subject  = "Welcome to Drug Testing Managment;
$message  ="Welcome to Drug Testing Managment. Please click on the link below to confirm your registration \n"
$message  = wordwrap($message, 70);
mail("webmaster@example.com",$subject,$message,"From: $from\n");

echo $jcod;
?>