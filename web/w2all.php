<?php
header("Content-type: application/json");
$jcode 		=	"[ ";
$comma		=	"";
$type       =   "a";
  if(isset( $_POST["str"]	)){ 
        $data 	= $_POST["str"]	; 
  }           
  if (empty($data ) )  
   if(isset( $_GET["str"]	)){ 
       $data 	= $_GET["str"]	; 
 }   
 if (empty($data ) ) 
 {    
     $data =       '{"actionid":"loginu","ret_code":"ok","rec_guid":"00000000-0000-0000-0000-000000000000","userguid":"00000000-0000-0000-0000-000000000000","time_out":"Fri, 20 Jun 2014 01:44:23 GMT","fieldefs":"","username":"test","password":"test123"}'  ;   
     echo "no data passed to regdtm using this fake data instead: \n  $data	\n\n";    
   }
$jparms		=	  json_decode($data);
if ($jparms->actionid == 'loginu' )
     $type      = "w";

     
     
     $sprocs	    =	  array(
 'loginu'   => "SP_Proces_tbl_fieldstyle"
,'addfld'   => "SP_Proces_tbl_userstyle",'delfld'   => "SP_Proces_tbl_userstyle",'fontsz'   => "SP_Proces_tbl_userstyle",'movfld'   => "SP_Proces_tbl_userstyle"
,'ordfld'   => "SP_Proces_tbl_userstyle",'rszfld'   => "SP_Proces_tbl_userstyle",'undfld'   => "SP_Proces_tbl_userstyle",'movgfd'   => "SP_Proces_tbl_userstyle"
,'font_b'   => "SP_Proces_tbl_userstyle",'bcolor'   => "SP_Proces_tbl_userstyle",'fcolor'   => "SP_Proces_tbl_userstyle",'italic'   => "SP_Proces_tbl_userstyle",'fontnm'   => "SP_Proces_tbl_userstyle"

,'regist'   => "SP_Proces_AddNewUsr",'revoke'   => "SP_Proces_AddNewUsr"
,'grantu'   => "SP_Proces_AddNewUsr",'begint'   => "SP_Proces_AddNewUsr"
,'disabl'   => "SP_Proces_AddNewUsr",'update'   => "SP_Proces_AddNewUsr"

,'ddogrids' => "SP_Proces_tbl_fieldstyle"  
,'drawdata' => "SP_Detail_tbl_labrawdata"
,'dcliacct' => "SP_Detail_tbl_clientacct"             
,'dclicont' => "SP_Detail_tbl_clientcont"             
,'dcliempl' => "SP_Detail_tbl_clientempl"                                                                        
,'dlabscol' => "SP_Detail_tbl_colectlabs"             
,'dcollabs' => "SP_Detail_tbl_colectlabs"             
,'dlabcont' => "SP_Detail_tbl_labcontact"             
,'dcollect' => "SP_Detail_tbl_colectcont"             
,'reporter' => "SP_Proces_tbl_reportdefs"               
,'dtpaclin' => "SP_Detail_tbl_tpa_contac"             
,'dtpacont' => "SP_Detail_tbl_tpa_contac"             
,'dlabtest' => "SP_Detail_tbl_testdetail"             
,'dmrocont' => "SP_Detail_tbl_mrocontact"    
,'dsecdetl' => "SP_Detail_tbl_mrocontact"   

,'pdogrids' => "SP_Popula_tbl_fieldstyle"  
,'pgetsecu' => "SP_Popula_tbl_security"
,'prawdata' => "SP_Popula_tbl_labrawdata"
,'preports' => "SP_Popula_tbl_rpt_detail" 
,'plabscol' => "SP_Popula_tbl_colectlabs"  
,'pcollabs' => "SP_Popula_tbl_colectlabs"  
,'plabcont' => "SP_Popula_tbl_labcontact"
,'pclients' => "SP_Popula_tbl_clientdata"
,'pcliacct' => "SP_Popula_tbl_clientacct"
,'pcolcols' => "SP_Popula_tbl_collectorz"
,'pclicont' => "SP_Popula_tbl_clientcont"                                                       
,'dgetsecu'  => "SP_Popula_tbl_security"              
,'pgetsecu'  => "SP_Popula_tbl_security" 

, 'sendmail' => "SP_Manage_tbl_emailupdat"                  
, 'searchin' => "SP_Manage_tbl_savesearch"
, 'searchnm' => "SP_Manage_tbl_savesearch"
, 'doupdate' => "SP_Manage_tbl_activetran"
, 'doinsert' => "SP_Manage_tbl_activetran"
, 'dodelete' => "SP_Manage_tbl_activetran"
, 'comitran' => "SP_Manage_tbl_activetran"
, 'deletran' => "SP_Manage_tbl_activetran"
,);



$f			    =	  fopen("debug_dtm_all.js", $type);
$all		    =	  $sprocs[$jparms->actionid] ;
$conn		    =	  sqlsrv_connect("192.168.2.2",array("UID"=>"sa" , "PWD"=>"w2w2w2w2" , "Database"=>"W2"));
$result		    =	  sqlsrv_query(  $conn, sprintf(" EXECUTE [W2].[dbo].[%s] @JSonStuff = N' %s ' ",$sprocs[$jparms->actionid] ,$data)   );
 while($row	    =	  sqlsrv_fetch_array( $result  ))  {
	  $row[0]	  =	  str_replace(array("\n", "\r"	), ' ',  $row[0]);
	  $row[0]	  =	  str_replace(array("'"), ' ',  $row[0]);
	  $jcode	  =	  $jcode  . $comma .  $row[0] ;
	  $comma	  =	  "," ;
 }
	$jcode	=	  $jcode . " ]";  fprintf($f,"data = %s\nall = %s\njcode = %s\n" , $data,$all,  $jcode  );   fclose($f);   	 	$list	=	  print_r(sqlsrv_errors());
    echo  $jcode ;
?>
