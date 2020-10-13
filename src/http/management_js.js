<meta http-equiv="Content-Type" content="text/html; charset="utf-8">
<html style="width:100%;height:100%;">
<body onload ="start()" style="width:100%;height:100%;">
<script>

var svg = false
var socket = false
var connWait = 0
var svgns = "http://www.w3.org/2000/svg"

function boundingbox(obj) {
    return obj.getBBox()
}


function softNode(parent, name, tree) {
   // look up in dom..this is n^2...there is some kind of query something something
   for (let elem of parent.children) {
      if (elem.name == name) {
         return elem
      }
   }
   // see if we can defer the attach...or maybe it doesnt matter
   obj = document.createElementNS(svgns, tree.kind);
   obj.name = name
   parent.appendChild(obj);
   return obj
}

// consider a shadow tree - I guess I'm just worried about namespace issues?
function set(dom, tree) {
    var obj = dom

    if (tree == null) {
        obj.remove()
        return
    }
    
    if ("children" in tree) {
        for (i in tree.children) {
           v = tree.children[i]
           n = softNode(dom, i, v) // right?
           set(n, v)
        }
    }

    for (var key in tree) {
        var k = tree[key]
	switch(key) {
        case 'children':
        case 'kind':
            break
	case 'click':
            // if k is empty then remove listener
            if (!("click" in obj)) {
                   rebind_k = function(x) {
                      x.addEventListener("click",
                                        function (evt) {putBatch(x.click)})
                   }
                   rebind_k(obj)
            } 
            obj.click = k
            break
	case 'text':
	    var textNode = document.createTextNode(tree.text)
	    obj.appendChild(textNode);
	    break;
	default: 
	    obj.setAttributeNS(null,key,k)
	}
    }
}


function clear() {
    if (svg != false) {
        svg.parentNode.removeChild(svg)
    }
    svg = document.createElementNS("http://www.w3.org/2000/svg","svg")
    svg.setAttributeNS(null, "width", "100%")
    svg.setAttributeNS(null, "height", "100%")
    document.body.appendChild(svg)
}

function send(item) {
     socket.send(JSON.stringify(item))
}

 
// this is the registration variant - maybe they are all* registration variants?
function getUpstream(path) {
     send({"read":path})
}

function putBatch(statements) {
    send({"write":statements})
}

function websocket(url) {  
    setTimeout(function() {
	socket = new WebSocket(url)
    socket.onopen = function(evt){
        clear()
        getUpstream("")
    }
    socket.onmessage = function(event){
            var msg = JSON.parse(event.data);
          set(svg, msg)
        }
    socket.onclose = 
            function(evt){
		svg.setAttributeNS(null, "fill", "grey") 
		connWait = connWait * 2 + 1000
		if (connWait > 5000) {
		    connWait = 5000
		}
		websocket(url)
	    }
    }, connWait)
}

function start() {
    terms = document.baseURI.split(':')
    terms[0] = 'ws'
    websocket(terms.join(":"))
}
</script>
</body>        
</html>
