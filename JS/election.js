//TODO: add sources (including http://bl.ocks.org/michellechandra/0b2ce4923dc9b5809922)
//TODO: add electoral differential map


var current_voter_map = {};
var current_electoral_map = {};
var moneyFormat = d3.format('$.1s');
var moneyFormat2 = d3.format('$,');
var commaFormat = d3.format(',');
var base_election_result = JSON.parse(JSON.stringify(election_tree))
var base_electoral_map = {};

var current_modifiers = [];

(function() {
  'use strict';

  $(document).ready(function() {


    function tooltipHtml(n, d){ /* function to create html content string in tooltip div. */
    if((n == "Nebraska" || n == "Maine") && (d.rep != 0 && d.dem !=0) && get_mapview() == "electoral"){
      return "<h4>"+n+"</h4><table>"+
        "<tr><td>Rep</td><td>"+(Math.round(d.rep).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","))+"</td></tr>"+
        "<tr><td>Dem</td><td>"+(Math.round(d.dem).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","))+"</td></tr>"+
        "</table><br><i>" + n + " has weird electoral rules</a></i>";
    } else {
      return "<h4>"+n+"</h4><table>"+
        "<tr><td>Rep</td><td>"+(Math.round(d.rep).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","))+"</td></tr>"+
        "<tr><td>Dem</td><td>"+(Math.round(d.dem).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","))+"</td></tr>"+
        "</table>";
    }
  }

  function drawMap() {
  
    var sampleData ={}; /* Sample random data. */
    var mapview = $('#mapview').val()

    Object.keys(electoral_college_tree)
      .forEach(function(d){ 
        if(mapview == "electoral"){
          var rep_electoral = current_electoral_map[d]["rep"];
          var dem_electoral = current_electoral_map[d]["dem"]; 
          sampleData[d]={rep:rep_electoral, dem:dem_electoral, 
               color:d3.interpolate("#ff0000", "#0000FF")(dem_electoral/(rep_electoral+dem_electoral))}; 
        } else if(mapview == "popular"){
          var rep_votes = 0
          var dem_votes = 0
          for(var i of Object.keys(current_voter_map[d])){
            rep_votes += current_voter_map[d][i]["Trump"][0]
            dem_votes += current_voter_map[d][i]["Clinton"][0]
          }
          sampleData[d]={rep:rep_votes, dem:dem_votes, 
            color:d3.interpolate("#ff0000", "#0000FF")(dem_votes/(rep_votes+dem_votes))}; 
        } else if(mapview == "demographic"){
          //we don't track modifier outcomes at a per-demographic level because we don't have
          //the right intersection data.  To show demographic preferences, we will have to
          //apply all the modifiers of the given demographic type.  To be more accurate,
          //we should also multiply by the ratio of total predicted votes over the sum
          //of our per-demographic computed voter sum.  e.g. if someone increases male turnout
          //and increases age 25-34 turnout, our deomographic-specific sum will be less than
          //our total, and we should account for that
          var demographic_type = $('#demographicviewtype').val()
          var demographic_value = $('#demographicviewvalue').val()
          var rep_votes = 0
          var dem_votes = 0
          
          for(var i of Object.keys(current_voter_map[d])){    
            var votes = get_demographic_votes(d, i, demographic_type, demographic_value);
            rep_votes += votes["rep"]
            dem_votes += votes["dem"]
          } 

          //now apply modifiers
          for(var modifier of current_modifiers){
            if(modifier["CATEGORY"] == demographic_type) {
              if(modifier["TYPE"] == "margin"){
                    var net_change = (votes["rep"] + votes["dem"]) * parseFloat(modifier["SHIFT"])/100.0/2.0
                    dem_votes += net_change
                    rep_votes -= net_change
                  } else if (modifier["TYPE"] == "turnout") {
                    var vote_multiple = parseFloat(modifier["SHIFT"])/100.0
                    dem_votes += vote_multiple * votes["dem"]
                    rep_votes += vote_multiple * votes["rep"]
                  }
              }
          }
        
          //now adjust values so we match the today votes in this state
          var total_votes = 0;
          var total_demographic_votes = 0;
          var demo_votes = {}

          for(var i of Object.keys(current_voter_map[d])){
            //for every district
            //total votes we think were made here
            total_votes += current_voter_map[d][i]["Trump"][0] + current_voter_map[d][i]["Clinton"][0]
            //now we need to iterate through every demo of our type, apply the relevant modifiers,
            //and add up the total
            for(var j of get_option_list(demographic_type)){
              var party_demo_votes = get_demographic_votes(d, i, demographic_type, j);
              demo_votes[j] = party_demo_votes["rep"] + party_demo_votes["dem"]
              for(var modifier of current_modifiers) {
                if (modifier["CATEGORY"] == demographic_type && modifier["DEMOGRAPHIC"] == j && modifier["TYPE"] == "turnout") {
                  var vote_multiple = parseFloat(modifier["SHIFT"])/100.0
                  demo_votes[j] += vote_multiple * demo_votes[j] 
                }
              }
              total_demographic_votes += demo_votes[j]
            }
          }
          var demographic_multiple = total_votes/total_demographic_votes;

          
          for(var i of Object.keys(current_voter_map[d])){
            dem_votes = dem_votes * demographic_multiple
            rep_votes = rep_votes * demographic_multiple
          }

          sampleData[d]={rep:rep_votes, dem:dem_votes, 
            color:d3.interpolate("#ff0000", "#0000FF")(dem_votes/(rep_votes+dem_votes))}; 
        } else if(mapview == "differential"){
          var rep_votes = 0
          var dem_votes = 0
          var rep_2016_votes = 0
          var dem_2016_votes = 0
          for(var i of Object.keys(current_voter_map[d])){
            rep_votes += current_voter_map[d][i]["Trump"][0]
            dem_votes += current_voter_map[d][i]["Clinton"][0]
            rep_2016_votes += base_election_result[d][i]["Trump"][0]
            dem_2016_votes += base_election_result[d][i]["Clinton"][0]
          } 

          var visual_multiple = 10
          //if the ratio of dems is higher than it used to be, shift blue.  Otherwise shift red
          sampleData[d]={rep:(rep_votes - rep_2016_votes), dem:(dem_votes - dem_2016_votes), 
              color: ((dem_votes/(dem_votes + rep_votes) > dem_2016_votes / (dem_2016_votes + rep_2016_votes)) ? 
                      d3.interpolate("#FFFFFF", "#0000FF")(visual_multiple * (dem_votes/(dem_votes + rep_votes) - dem_2016_votes / (dem_2016_votes + rep_2016_votes))) :
                      d3.interpolate("#FFFFFF", "#FF0000")(visual_multiple * (dem_2016_votes / (dem_2016_votes + rep_2016_votes) - dem_votes/(dem_votes + rep_votes))))
            }; 
        } else if(mapview == "electoral-differential"){
          
          var rep_electoral = current_electoral_map[d]["rep"];
          var dem_electoral = current_electoral_map[d]["dem"]; 
          var base_rep_electoral = base_electoral_map[d]["rep"];
          var base_dem_electoral = base_electoral_map[d]["dem"]; 
          
          sampleData[d]={rep:(rep_electoral - base_rep_electoral), dem:(dem_electoral - base_dem_electoral), 
              color: ((rep_electoral > base_rep_electoral) ? 
                      d3.color("#FF0000") :
                        (dem_electoral > base_dem_electoral) ?
                        d3.color("#0000FF") :
                        d3.color("#FFFFFF"))
            }; 
        }
      });
    
      /* draw states on id #statesvg */ 
      uStates.draw("#statesvg", sampleData, tooltipHtml);
      
      d3.select(self.frameElement).style("height", "600px"); 
   }

    function populateInput(){
      var input = {};

      var inputs = document.getElementsByClassName('ms-input');
      for (var i=0; i < inputs.length; i++) {
        input[inputs[i].name] = inputs[i].value;
      }
    
      return input;
    }
    function get_demographic_votes_from_modifier(state, cd, modifier){
      return get_demographic_votes(state, cd, modifier["CATEGORY"], modifier["DEMOGRAPHIC"]);
    }
    function get_demographic_votes(state, cd, category, demographic){
      var current_votes = { 
        "rep" : demographic_tree[state][cd][category][demographic]["InterpolatedTrump"][0],
        "dem" : demographic_tree[state][cd][category][demographic]["InterpolatedClinton"][0]
      }
      return current_votes
    }
    function get_district_votes(election_result, state, cd){
      var current_votes = {
          "dem" : election_result[state][cd]["Clinton"][0],
          "rep" : election_result[state][cd]["Trump"][0]
        }
      return current_votes;
    }
    function get_state_votes(election_result, state){
      var current_votes = {
        "dem" : 0,
        "rep": 0
      };
      for(var cd of Object.keys(election_result[state])){
        var votes = get_district_votes(election_result, state, cd)
        current_votes["dem"] += votes["dem"];
        current_votes["rep"] += votes["rep"];
      }
      return current_votes;
    }
    function get_electoral_college_votes(state){
      return electoral_college_tree[state][0];
    }
    function evaluate_election(aggregation="electoral"){
      //deep clone the object.  Apparently this is the fastest way, because Javascript is a joke
      var election_result = JSON.parse(JSON.stringify(election_tree))
      for(var modifier of current_modifiers){
        for(var state of Object.keys(election_result)) {
          for(var cd of Object.keys(election_result[state])){
            var votes = get_demographic_votes_from_modifier(state, cd, modifier)
            /*
              So we could try to track demographic vote changes at a per-demographic level.  The
              problem is if you increase e.g. male turnout by 5%, you don't know how to adjust
              the age buckets, because we don't have the full n-dimensional population intersection
              array.

              If we tracked at the deomgraphic level instead of the aggregate level, we'd end up
              summing the AGE demographic votes and summing the GENDER demographic votes and getting
              different numbers.
            */
            if(modifier["TYPE"] == "margin"){
              var net_change = (votes["rep"] + votes["dem"]) * parseFloat(modifier["SHIFT"])/100.0/2.0
              election_result[state][cd]["Clinton"][0] += net_change
              election_result[state][cd]["Trump"][0] -= net_change
            } else if (modifier["TYPE"] == "turnout") {
              var vote_multiple = parseFloat(modifier["SHIFT"])/100.0
              election_result[state][cd]["Clinton"][0] += vote_multiple * votes["dem"]
              election_result[state][cd]["Trump"][0] += vote_multiple * votes["rep"]
            }
          }
        }
      }
      //store it globally so we can access it from our map
      current_voter_map = election_result;
      current_electoral_map = {}
      if(aggregation == "electoral") {
        var dem_total = 0
        var rep_total = 0
        for(var state of Object.keys(election_result)){
          current_electoral_map[state] = {}
          votes = get_state_votes(election_result, state);
          current_electoral_map[state]["rep"] = 0
          current_electoral_map[state]["dem"] = 0
            
          if(state == "Maine" || state == "Nebraska"){
            //"Maine and Nebraska ... allocate two electoral votes to the state 
            //popular vote winner, and then one electoral vote to the popular 
            //vote winner in each Congressional district"
            //http://www.270towin.com/content/split-electoral-votes-maine-and-nebraska/
            if(votes["rep"] > votes["dem"]){
              rep_total = rep_total + 2
              current_electoral_map[state]["rep"] = 2
            } else {
              dem_total = dem_total + 2
              current_electoral_map[state]["dem"] = 2
            }

            for(var cd of Object.keys(election_result[state])){
              votes = get_district_votes(election_result, state, cd);
              if(votes["rep"] > votes["dem"]){
                rep_total = rep_total + 1
                current_electoral_map[state]["rep"] += 1
              } else {
                dem_total = dem_total + 1
                current_electoral_map[state]["dem"] += 1
              }
            }
          }
          else if(votes["rep"] > votes["dem"]){
            var electoral_votes = get_electoral_college_votes(state)
            rep_total = rep_total + electoral_votes
            current_electoral_map[state]["rep"] = electoral_votes
          } else {
            var electoral_votes = get_electoral_college_votes(state)
            dem_total = dem_total + electoral_votes
            current_electoral_map[state]["dem"] = electoral_votes
          }
        }
        //if there are no modifiers, store this electoral map globally so we can
        //access it and do a diff later.  This happens once at the beginning of execution,
        //so this map should always be filled out.
        if (current_modifiers.length == 0){
          base_electoral_map = JSON.parse(JSON.stringify(current_electoral_map))
        }
        return({"rep.votes" : rep_total, "dem.votes" : dem_total})
      } else
      {
        return("ERROR")
      }
    }

    function predict_text(prediction)
    {
      var result = "";
      var index = 0;
      if(current_modifiers.length > 0){
        result += "If we: <ul>";
      }/* else if(current_modifiers.length == 1){
        result += "If we ";
      }*/
      for(input of current_modifiers){
        //if(current_modifiers.length > 1){
        result+="<li>"
        //}
        if(input["TYPE"] == "turnout")
        {
          if(input["SHIFT"] > 0){
            result += "increase the turnout of "
          } else if (input["SHIFT"] < 0){
            result += "decrease the turnout of "
          } else {
            result += "leave the turnout of "
          }
          if(input["CATEGORY"] == "age"){
            result += " aged "
          }
          result += input["DEMOGRAPHIC"] + " ";
          if(input["SHIFT"] == 0){
            result += "voters unchanged "
          } else { 
            result += "voters by " + Math.abs(input["SHIFT"]) + " points"
          }
        } else if(input["TYPE"] == "margin"){
          if(input["SHIFT"] == 0){
            result += "leave the margin of "
          } else {
            result += "shift the margin of "
          }
           
          if(input["CATEGORY"] == "age"){
            result += " aged "
          }
          result += input["DEMOGRAPHIC"] + " ";
          result += "voters "

          if(input["SHIFT"] > 0){
            result += "by " + input["SHIFT"] + " points toward Democrats "
          } else if(input["SHIFT"] < 0){
            result += "by " + Math.abs(input["SHIFT"]) + " points toward Republicans "
          } else {
            result += "unchanged "
          }
        }
        result += "<i class='fa fa-close delete_modifier' name='" + index + "'></i>";
        //if(current_modifiers.length > 1){
        result+="</li>"
        //}
        index += 1;
      }
      //if(current_modifiers.length > 1){
      result+="</ul>"
      //  }
      if(prediction["rep.votes"] > prediction["dem.votes"]){
        result += "Republicans win " + prediction["rep.votes"] + " to " + prediction["dem.votes"] + " electoral votes"  
      } else if(prediction["rep.votes"] < prediction["dem.votes"]){
        result += "Democrats win " + prediction["dem.votes"] + " to " + prediction["rep.votes"] + " electoral votes"  
      }
      return result;
    }
    function predict() {
      var evaluated_election = evaluate_election("electoral")
      return evaluated_election;
    }
    function append_and_run()
    {
      var input = populateInput();
      current_modifiers.push(input);
      run();
    }
    function run() { 
      console.log(current_modifiers)
      var pred = predict();
      var pred_text = predict_text(pred);

      // document.getElementById('pred').innerHTML = 'Lognormal distribution mean: ' + pred;
      // document.getElementById('varpred').innerHTML = 'Lognormal distribution standard deviation: ' + varpred;
      document.getElementById('result').innerHTML = pred_text;

      var delete_modifiers = document.getElementsByClassName('delete_modifier');
      for(var delete_modifier of delete_modifiers){
        delete_modifier.addEventListener('click', delete_modifier_from_current);     
      }
      update_type_ui();
     
      drawMap();
    }
    function delete_modifier_from_current(delete_event){
      var index = parseInt(delete_event.target.getAttribute("name"))
      current_modifiers.splice(index, 1);
      run();
    }

    function get_modifier_category(){
      return document.getElementsByName("CATEGORY")[0].value
    }
    function get_modifier_type(){
     return document.getElementsByName("TYPE")[0].value
    }
    function get_mapview_category(){
      return document.getElementById("demographicviewtype").value
    }
    function get_mapview(){
      return document.getElementById("mapview").value
    }
    function get_option_list(demographic) {
      if(demographic == "gender") {
          return ["Male", "Female"];
      } else if (demographic == "race"){
          return ["White", "Hispanic or Latino (of any race)", 
          "Black or African American", "American Indian and Alaska Native",
          "Asian", "Native Hawaiian and Other Pacific Islander", 
          "Some other race", "Two or more races"];
      } else if (demographic == "age"){
        return ["15 to 19 years", "20 to 24 years", 
          "25 to 34 years", "35 to 44 years", "45 to 54 years", 
          "55 to 59 years", "60 to 64 years", "65 to 74 years", 
          "75 to 84 years", "85 years and over"];
      }
    }
    function update_modifier_demographic_options(){
      var new_options = []
      $('#DEMOGRAPHIC').empty();
      new_options = get_option_list(get_modifier_category())
      $.each(new_options, function(index, value) {
         
         $('#DEMOGRAPHIC')
         .append($("<option></option>")
                    .attr("value",value) //.toLowerCase()
                    .text(value)); 
      });
    }
    function update_map_demographic_options(){
      var new_options = []
      $('#demographicviewvalue').empty();
      new_options = get_option_list(get_mapview_category())
      $.each(new_options, function(index, value) {
         $('#demographicviewvalue')
         .append($("<option></option>")
                    .attr("value",value) //.toLowerCase()
                    .text(value)); 
       })
      //if we just refilled the demogrpahic view options, we need to reset the value
      if (get_mapview_category() == get_modifier_category()) {
        $('#demographicviewvalue').val(document.getElementsByName("DEMOGRAPHIC")[0].value)
      } //otherwise, we just wiped away the value and defaulted to something OTHER than
      //the current modifier, which means we manually swapped the viewtype to something else
    }

     function update_type_ui(){
      console.log("updating ui...")

      if(get_modifier_type() == "turnout") {
        $('#shift_label_prefix').text("Lower Turnout");
        $('#shift_label_postfix').text("Higher Turnout");
      } else if (get_modifier_type() == "margin"){
        $('#shift_label_prefix').html("More <span style='color:red'>Republican</span>");
        $('#shift_label_postfix').html("More <span style='color:blue'>Democrat</span>");
      } else{
        $('#shift_label_prefix').text("");
        $('#shift_label_postfix').text("");
      }
    }

    var inputs = document.getElementsByClassName('ms-input');
    for (var i=0; i < inputs.length; i++) {
      if (inputs[i].type == 'range') {
        // not sure why data-attr isn't working for SALARY, 
        // I need to investigate this later.
        var from = inputs[i].value; 
        $(inputs[i]).ionRangeSlider({
          from: from,
          onFinish: function (data) {
            data.input.context.value = data.from;
          },
        });
      } 
    }

    function demographicviewtypeChange()
    {
      update_map_demographic_options();
      drawMap(); 
    }

    function mapviewChange(){
      if ( $('#mapview').val() == "demographic")
      {
        $('#demographicviewtypediv').attr("hidden", false)
        $('#demographicviewvaluediv').attr("hidden", false)

        //If we've reached this point, someone just changed us to demographic view, so set the
        //default category and value to whatever the current modifier looks like
        $('#demographicviewtype').val(document.getElementsByName("CATEGORY")[0].value)         
        $('#demographicviewvalue').val(document.getElementsByName("DEMOGRAPHIC")[0].value);      
      } else {
        $('#demographicviewtypediv').attr("hidden", true)
        $('#demographicviewvaluediv').attr("hidden", true)
      }
      update_map_demographic_options();
      drawMap();
    }
    function demographicSelectorChange(){
      drawMap();
    }

    // add event listeners for all the input elements
    var input = document.getElementsByClassName('ms-input-submit')[0];
    input.addEventListener('click', append_and_run);   

    var modifier_selector = document.getElementsByClassName('ms-input-category')[0];
    modifier_selector.addEventListener('change', update_modifier_demographic_options);   
    
    var type_selector = document.getElementsByClassName('ms-input-type')[0];
    type_selector.addEventListener('change', update_type_ui);   
    
    var mapview_selector = document.getElementById('mapview');
    mapview_selector.addEventListener('change', mapviewChange);   
    
    var mapview_demographic_type_selector = document.getElementById('demographicviewtype');
    mapview_demographic_type_selector.addEventListener('change', demographicviewtypeChange);   

    var mapview_demographic_value_selector = document.getElementById('demographicviewvalue');
    mapview_demographic_value_selector.addEventListener('change', demographicSelectorChange);   
    // run once on load
    //update_type_ui();
    //drawMap();
    run();
  });
})();