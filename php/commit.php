<?php

$re = '/(?#! splitCamelCase Rev:20140412)
    # Split camelCase "words". Two global alternatives. Either g1of2:
      (?<=[a-z])      # Position is after a lowercase,
      (?=[A-Z])       # and before an uppercase letter.
    | (?<=[A-Z])      # Or g2of2; Position is after uppercase,
      (?=[A-Z][a-z])  # and before upper-then-lower case.
    /x';


function recursiveShort ($parts) {
        $head = array_shift($parts);
        $heads[$head] = true;

        # science term stemming... or2
        if (!preg_match('/ility$/', $head) && preg_match('/ity$/', $head)) {
                $head_stemmed = preg_replace('/ity$/', 'e', $head);
                $heads[$head_stemmed] = true;
        }

        if (strlen($head) >= 5) {
                $heads[substr($head, 0, 1)] = true;
                $heads[substr($head, 0, 2)] = true;
                $heads[substr($head, 0, 3)] = true;
                $heads[substr($head, 0, 4)] = true;
                $heads[substr($head, 0, 5)] = true;
                $heads[substr($head, 0, 2) . $head[strlen($head)-1]] = true;
                if (!preg_match('/^[aeiouAEIOU]/', $head)) {
                        $mm = str_replace(array("a", "e", "i", "o", "u", "A", "E", "I", "O", "U"), "", $head);
                        $heads[$mm] = true;
                        if (strlen($mm) >= 3) {
                                $heads[substr($mm, 0, 3)] = true;
                                $heads[substr($mm, 0, 2) . $head[strlen($mm)-1]] = true;
                        }
                }
        }
        else if (strlen($head) == 4) {
                if (!preg_match('/^[aeiouAEIOU]/', $head)) {
                        $mm = str_replace(array("a", "e", "i", "o", "u", "A", "E", "I", "O", "U"), "", $head);
                        if (strlen($mm) == 3) {
                                $heads[$mm] = true;
                        }
                }
        }

        if (count($parts) > 0) {
                $heads[substr($head, 0, 1)] = true;
                $rest = recursiveShort($parts);
        }
        else {
                return array_keys($heads);
        }

        $ret = array();
        foreach ($heads as $h => $dum) {
                foreach ($rest as $r) {
                        $ret[] = $h . $r;
                }
        }

        return $ret;

}




$j = json_decode(file_get_contents("tosolr.json"));
foreach ($j as $jid => $d) {

        $a = preg_split($re, $d->term);
        if (count($a) <= 4) {
                $comb = recursiveShort($a);
        }

        $ffss[$jid]['desc'] .= implode(" ", $comb) . " " . $d->desc;
        $ffss[$jid]['desc_orig_s'] = $d->desc;
        $ffss[$jid]['term'] = $d->term;
        $ffss[$jid]['vocab'] = $d->vocab;
        $ffss[$jid]['id'] = md5($d->vocab . ":" . $d->term);
}


file_put_contents("tosolr.mod.json", json_encode($ffss));

$jf = "curl 'http://your.host/solr/update/json?commit=true' --data-binary @tosolr.mod.json -H 'Content-type:application/json'";
$response = exec($jf, $out);


?>