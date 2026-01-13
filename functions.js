
/*

*/
function setDepartment(ev)
{
    console.log("setDepartment");
    console.log(ev);    
    var div = document.getElementById("abstracts");
    div.innerHTML = ""

    console.log("department = " + ev.target.value);
    console.log("abs1 = " + Object.keys(byDept[ev.target.value]));
    console.log("abs1 = " + byDept[ev.target.value].length);
    console.log("abs1 = " + Object.keys(byDept[ev.target.value][0]));            
 //   byDept[
    showAbstracts(byDept[ev.target.value]);
}



function mkDeptOptions(vals)
{
    var h = "";
    vals.forEach(function(item, index) {
	h = h + "<option value='" + item + "'>" + item + "</option>"
    });
    return(h);
}

function showAbstracts(a)
{
    var div = document.getElementById("abstracts");
    div.innerHTML = "";
    var h = "";
    a.forEach(function(item, index) {
        h = h + mkAbstract(item);
    });
    div.innerHTML = h;
}

function mkAbstract(a)
{

    //  title,studentName,sponsor,dept,abstract,row,column,pageNum,year

    return("<div class='abstract'>" +  
            "<h3>" + a.title + "</h3>" +
	   "<table><tr><th>Student</th><th>Mentor/Sponsor</th><th>Year</th><th>Page</th></tr><tr>" +
	   "<th>" + a.studentName + "</th>" + 
	   "<th>" + a.sponsor + "</th>"  +
	   "<th>" + a.year + "</th>" +
	   "<th>" + a.pageNum + "</th>"  +
	   "</tr></table>" + 
//	   "<h5> Student: " + a.studentName + "</h5>" + 
//	   "<h5> Sponsor: " + a.sponsor + "</h5>"  +
//	    "<h6> " + a.year + " page " + a.pageNum + "</h6>"  +	   
            "<p class='content'>" + a.abstract + "</p>" + 
            "</div>");

}
