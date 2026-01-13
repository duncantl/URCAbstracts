
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
	h += "<option value='" + item + "'>" + item + " (" + byDept[item].length + ")</option>";
    });
    return(h);
}

var CountsByYear = {};
function showAbstracts(a)
{
    var div = document.getElementById("abstracts");
    div.innerHTML = "";
    var curYear = a[0].year;
    var years = [ curYear];
    var ctr = 0;
    CountsByYear = {};
    
    var h = "<h3 id='" + curYear + "'>" + curYear + "</h3>";
    
    a.forEach(function(item, index) {
	if(item.year != curYear) {
	    CountsByYear[ curYear + "" ] = ctr;
	    ctr = 0;
	    curYear = item.year;
	    years.push(curYear);
	    h = h + "<hr width=50% /> <h1 id='" + curYear + "'>" + curYear + "</h1>";
	}

	ctr++;
	
        h = h + mkAbstract(item);
    });

    CountsByYear[ curYear + "" ] = ctr;

    console.log(CountsByYear);
    
    div.innerHTML = mkYearLinks(years) + h;
}

function mkYearLinks(years)
{
    var txt = "<p>";
    years.forEach(function(item, index) {    
	txt += "<a href=#" + item + ">" + item + "</a> (" +  CountsByYear[ item + "" ] + ")&nbsp;";
    });
    txt += "</p>"
    return(txt);
}

function mkAbstract(a)
{

    //  title,studentName,sponsor,dept,abstract,row,column,pageNum,year

    return("<div class='abstract'>" +  
           "<h3>" + a.title + "</h3>" +
	   "<table><tr><th>Mentor/Sponsor</th><th>Student</th><th>Year</th><th>Page</th></tr><tr>" +
	   "<td>" + a.sponsor + "</td>"  +
	   "<td>" + a.studentName + "</td>" + 
	   "<td><a href='" + getPDFURL(a.year, a.pageNum)  + "'>" + a.year + "</a></td>" +
	   "<td>" + a.pageNum + "</td>"  +
	   "</tr></table>" + 
           "<p class='content'>" + a.abstract + "</p>" + 
           "</div>");

}

var PDFURLs = {"2019": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/inline-files/2019%20URC%20Abstract%20Book%20V.4%5B1%5D.pdf",
	       "2020": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/inline-files/2020%20URC%20Abstract%20Book%20V.3%20%28WEB%29_0.pdf",
	       "2021": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/inline-files/2021%20URC%20CONF%20Abstract%20FINAL.pdf",
	       "2023": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/inline-files/2023%20URC%20Abstract%20Book%20V.3_0_0.pdf",
	       "2024": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/media/documents/2024-abstract-book.pdf",
	       "2025": "https://urc.ucdavis.edu/sites/g/files/dgvnsk3561/files/media/documents/25_URC_Abstract_Book_V2.pdf"
	      };

function getPDFURL(year, page)
{
    return(PDFURLs[ year + "" ] + "#page=" + page);
}
