
//TODO- It is sloppy to keep all this stuff in the global namespace....  Fix this up when you get a chance.

var global_target;

function hideTelInput() {
    telInput.style.display="none";
}

function showTelInput(target) {
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
    return <td><button onclick={"onPress('" + value + "')"}><span class="number">{value}</span><span class="letter">{subValue}</span></button></td>;
}

var telephoneWidget = 

    <div id='telInput' style='display: none; position: absolute; left: 0px; top: 50px; border: solid black 1px; padding: 10px; background-color: rgb(200,100,100); text-align: justify; font-size: 12px; width: 135px;' onmouseover="document.getElementById('PopUp').style.display = 'none' ">
      <table class="phone">
	<colgroup>
	  <col style="width: 33%" />
	  <col style="width: 33%" />
	  <col style="width: 33%" />
	</colgroup>
	<tr>
	  <td><button onclick="onPress('1')"><span class="number">1</span><span class="letter">_</span></button></td>
	  <td><button onclick="onPress('2')" style="text-align: center;"><span class="number">2</span><span class="letter">ABC</span></button></td>
	  <td><button onclick="onPress('3')"><span class="number">3</span><span class="letter">DEF</span></button></td>
	</tr>
	<tr>
	  <td><button onclick="onPress('4')"><span class="number">4</span><span class="letter">GHI</span></button></td>
	  <td><button onclick="onPress('5')"><span class="number">5</span><span class="letter">JKL</span></button></td>
	  <td><button onclick="onPress('6')"><span class="number">6</span><span class="letter">MNO</span></button></td>
	</tr>
	<tr>
	  <td><button onclick="onPress('7')"><span class="number">7</span><span class="letter">PQRS</span></button></td>
	  <td><button onclick="onPress('8')"><span class="number">8</span><span class="letter">TUV</span></button></td>
	  <td><button onclick="onPress('9')"><span class="number">9</span><span class="letter">WXYZ</span></button></td>
	</tr>
	<tr>
	  <td><button onclick="onPress('*')"><span class="number">*</span></button></td>
	  <td><button onclick="onPress('0')"><span class="number">0</span></button></td>
	  <td><button onclick="onPress('#')"><span class="number">#</span></button></td>
	</tr>
      </table>
    </div>;


addEventListener("load", function() {

    document.body.appendChild(telephoneWidget);

});
