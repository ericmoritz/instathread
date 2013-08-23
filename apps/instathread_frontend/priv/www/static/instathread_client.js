var hateaos = {}

hateaos.client = function(url, callback) {
    jQuery.get(url, hateaos.makeClientCallback(url, callback));
}

hateaos.makeClient = function(url, html, callback) {
    var $html = jQuery(html);
    var client = {url: url};

    // for each link, extend the client with a link ajax function
    $html.find("link").each(function(i, link) {
	_.extend(client, hateaos.makeLinkFunction(link));
    });

    // for each form, extend the client with the form post function
    $html.find("form").each(function(i, form) {
	_.extend(client, hateaos.makeFormFunction(form));
    });

    // for each anchor, extend the client with an anchor function
    $html.find("a").each(function(i, a) {
	_.extend(client, hateaos.makeAFunction(a));
    });

    callback(client, html);
};

hateaos.makeClientCallback = function(url, callback) {
    return function(html) {
	hateaos.makeClient(url, html, callback);
    }
}

hateaos.makeClientFunction = function(name, fun) {
    var ret = {}
    ret[name] = fun;
    return ret;
}

hateaos.makeAFunction = function(a) {
    var $a = jQuery(a);
    var name = $a.attr("id");
    return hateaos.makeClientFunction(
	name, 
	function(callback) {
	    var url = $a.attr("href");
	    jQeury.get(
		url,
		hateaos.makeClientCallback(url, callback)
	    )
	}
    );
};

hateaos.makeFormFunction = function(form) {
    var $form = jQuery(form);
    var name = $form.attr("id");
    return hateaos.makeClientFunction(
	name, 
	function(fields, callback) {
	    var url = $form.attr("action");
	    jQuery.post(
		url,
		fields, 
		hateaos.makeClientCallback(url, callback)
	    )
	}
    );

}

hateaos.makeLinkFunction = function(link) {
    var $link = jQuery(link);
    var name = $link.attr("id");
    return hateaos.makeClientFunction(
	name,
	function(fields, callback) {
	    hateaos.getURITemplate(
		$link.attr("href"), 
		fields,
		callback
	    );
	}
    );
}

hateaos.getURITemplate = function (uri_template, fields, callback) {
    var template = UriTemplate.parse(uri_template);
    var uri = template.expand(fields);
    jQuery.get(uri, hateaos.makeClientCallback(uri, callback));
}
