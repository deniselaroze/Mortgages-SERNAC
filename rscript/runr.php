<?php
error_reporting(E_ALL & ~E_NOTICE);
header('Content-Type: text/html; charset=utf-8');

$genderQ = getGender();
$econQ = getEcon();
$mode1Q = getMode1Q();
$mode2Q = getMode2Q();
$pgQ = getPg();
$idQ = getId();

// execute R script from shell

#logConsole('genderQ', $genderQ, true);
#logConsole('econQ', $econQ, true);
#logConsole('mode1Q', $mode1Q, true);
#logConsole('mode2Q', $mode2Q, true);
#logConsole('pgQ', $pgQ, true);
#logConsole('idQ', $idQ, true);

$command = "Rscript /var/www/r.cess.cl/public_html/Treatment-Script-Server-block.R $genderQ $econQ $mode1Q $mode2Q $pgQ $idQ";
$out = trim(shell_exec($command));




#echo(explode(',', $out));
$pagos = explode(',', $out);

#logConsole('pagos', $pagos, true);


#header('Content-Type: application/json');
#echo(json_encode($pays));

echo "pay10=" . $pagos[0] . "&";
echo "pay11=" . $pagos[1] . "&";
echo "pay12=" . $pagos[2] . "&";
echo "pay13=" . $pagos[3] . "&";
echo "pay14=" . $pagos[4] . "&";
echo "pay15=" . $pagos[5] . "&";
echo "pay16=" . $pagos[6] . "&";
echo "pay17=" . $pagos[7] . "&";
echo "pay18=" . $pagos[8] . "&";
echo "pay19=" . $pagos[9] . "&";
echo "pay110=" . $pagos[10] . "&";
echo "pay111=" . $pagos[11] . "&";
echo "pay112=" . $pagos[12] . "&";
echo "pay113=" . $pagos[13] . "&";
echo "pay114=" . $pagos[14] . "&";
echo "pay115=" . $pagos[15] . "&";
echo "pay21=" . $pagos[16] . "&";
echo "pay22=" . $pagos[17] . "&";
echo "pay23=" . $pagos[18] . "&";
echo "pay24=" . $pagos[19] . "&";
echo "pay25=" . $pagos[20] . "&";
echo "pay26=" . $pagos[21] . "&";
echo "pay27=" . $pagos[22] . "&";
echo "pay28=" . $pagos[23] . "&";
echo "pay29=" . $pagos[24] . "&";
echo "pay210=" . $pagos[25] . "&";
echo "pay211=" . $pagos[26] . "&";
echo "pay212=" . $pagos[27] . "&";
echo "pay213=" . $pagos[28] . "&";
echo "pay214=" . $pagos[29] . "&";
echo "treatv1=" . $pagos[30] . "&";
echo "treatv2=" . $pagos[31];



#======================================================================================

function getGender(){
	if(isset($_GET['genderQ'])){
		$str = trim($_GET['genderQ']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getEcon(){
	if(isset($_GET['econQ'])){
		$str = trim($_GET['econQ']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getMode1Q(){
	if(isset($_GET['mode1Q'])){
		$str = trim($_GET['mode1Q']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getMode2Q(){
	if(isset($_GET['mode2Q'])){
		$str = trim($_GET['mode2Q']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getPg(){
	if(isset($_GET['pgQ'])){
		$str = trim($_GET['pgQ']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getId(){
	if(isset($_GET['idQ'])){
		$str = trim($_GET['idQ']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function writeLog($msg){

	@file_put_contents('./runr.log', date('Y-m-d h:i:s') . "\t" . $genderQ . "\t" . $msg . "\n");
}

function logConsole($name, $data = NULL, $jsEval = FALSE)
 {
      if (! $name) return false;

      $isevaled = false;
      $type = ($data || gettype($data)) ? 'Type: ' . gettype($data) : '';

      if ($jsEval && (is_array($data) || is_object($data)))
      {
           $data = 'eval(' . preg_replace('#[\s\r\n\t\0\x0B]+#', '', json_encode($data)) . ')';
           $isevaled = true;
      }
      else
      {
           $data = json_encode($data);
      }

      # sanitalize
      $data = $data ? $data : '';
      $search_array = array("#'#", '#""#', "#''#", "#\n#", "#\r\n#");
      $replace_array = array('"', '', '', '\\n', '\\n');
      $data = preg_replace($search_array,  $replace_array, $data);
      $data = ltrim(rtrim($data, '"'), '"');
      $data = $isevaled ? $data : ($data[0] === "'") ? $data : "'" . $data . "'";

$js = <<<JSCODE
\n<script>
 // fallback - to deal with IE (or browsers that don't have console)
 if (! window.console) console = {};
 console.log = console.log || function(name, data){};
 // end of fallback

 console.log('$name');
 console.log('------------------------------------------');
 console.log('$type');
 console.log($data);
 console.log('\\n');
</script>
JSCODE;

      echo $js;
 } 

?>
