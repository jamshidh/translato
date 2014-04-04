
//TODO- It is sloppy to keep all this stuff in the global namespace....  Fix this up when you get a chance.

var global_target;

function hideTelInput() {
    telInput.style.display="none";
}

function showTelInput(target) {
    global_target = target;
    var rect = target.getBoundingClientRect();

    //TODO- standardize this and put it in a shim
    var scrollX = (window.pageXOffset !== undefined) ? window.pageXOffset : (document.documentElement || document.body.parentNode || document.body).scrollLeft;
    var scrollY = (window.pageYOffset !== undefined) ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body).scrollTop;

    telInput.style.left = scrollX + rect.left + 10 + "px";
    telInput.style.top = scrollY + rect.bottom + "px";
    telInput.style.display="block";
}

function getParents() {
    
}

function checkOnFocus(target) {
    setTimeout(function () {
	showTelInput(target);
    });
}

function checkOnBlur() {
    setTimeout(function () {
	var node = document.activeElement;
	while(node) {
	    if (node == telInput) { 
	        global_target.focus();
	        return;
	    }
	    node = node.parentElement;
	}
	
	
	hideTelInput();
	
    });
}

function onPress(val) {
    global_target.value += val;
}

function makeButton(value, subValue) {
    return <td><button onclick={"onPress('" + value + "')"}><img src={"/res/addTelephoneInput/" + value + ".png"} /></button></td>;
}

var telephoneWidget = 
    <div id='telInput'>
      <table class="phone">
	<colgroup>
	  <col style="width: 33%" />
	  <col style="width: 33%" />
	  <col style="width: 33%" />
	</colgroup>
	<tr>
          <td><button onclick="onPress('1')"><img src={"/res/addTelInput/1.png"} /></button></td>
          <td><button onclick="onPress('2')"><img src={"/res/addTelInput/2.png"} /></button></td>
          <td><button onclick="onPress('3')"><img src={"/res/addTelInput/3.png"} /></button></td>
	</tr>
	<tr>
          <td><button onclick="onPress('4')"><img src={"/res/addTelInput/4.png"} /></button></td>
          <td><button onclick="onPress('5')"><img src={"/res/addTelInput/5.png"} /></button></td>
          <td><button onclick="onPress('6')"><img src={"/res/addTelInput/6.png"} /></button></td>
	</tr>
	<tr>
          <td><button onclick="onPress('7')"><img src={"/res/addTelInput/7.png"} /></button></td>
          <td><button onclick="onPress('8')"><img src={"/res/addTelInput/8.png"} /></button></td>
          <td><button onclick="onPress('9')"><img src={"/res/addTelInput/9.png"} /></button></td>
	</tr>
	<tr>
          <td><button onclick="onPress('*')"><img src={"/res/addTelInput/star.png"} /></button></td>
          <td><button onclick="onPress('0')"><img src={"/res/addTelInput/0.png"} /></button></td>
          <td><button onclick="onPress('#')"><img src={"/res/addTelInput/pound.png"} /></button></td>
	</tr>
      </table>
    </div>;


addEventListener("load", function() {

    document.body.appendChild(telephoneWidget);

});
