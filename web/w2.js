 
(function ($) {		 
	var browser = $.browser
	if (!browser) { 	function uaMatch( ua ) {	ua = ua.toLowerCase();
							var match = /(chrome)[ \/]([\w.]+)/.exec( ua ) || /(webkit)[ \/]([\w.]+)/.exec( ua ) || /(opera)(?:.*version|)[ \/]([\w.]+)/.exec( ua ) || /(msie) ([\w.]+)/.exec( ua ) || ua.indexOf("compatible") < 0 && /(mozilla)(?:.*? rv:([\w.]+)|)/.exec( ua ) || [];
								return { 	browser: match[ 1 ] || "",	version: match[ 2 ] || "0"		};
							};
							var matched = uaMatch( navigator.userAgent );
							browser = {};
							if ( matched.browser ) { 		browser[ matched.browser ] = true; 	browser.version = matched.version; 	}
							if ( browser.chrome )   		browser.webkit = true;
					  else  if ( browser.webkit ) 			browser.safari = true;
	}
    if(typeof $.support.selectstart != 'function')		$.support.selectstart = "onselectstart" in document.createElement("div");
    if(typeof $.fn.disableSelection != 'function')		$.fn.disableSelection = function() {   return this.bind( ( $.support.selectstart ? "selectstart" : "mousedown" ) +   ".ui-disableSelection", function( event ) {   event.preventDefault();  });    };
	$.support.htmlMenuitem = ('HTMLMenuItemElement' in window);
	$.support.htmlCommand = ('HTMLCommandElement'   in window);
	if (!$.ui || !$.ui.widget) {var _cleanData = $.cleanData;
		$.cleanData = function( elems ) {	 for ( var i = 0, elem; (elem = elems[i]) != null; i++ ) {	try {	$( elem ).triggerHandler( "remove" );	} catch( e ) {}}
												_cleanData( elems ); };
	}
$.addFlex			= function (t, pp) {	if (t.grid	) return false; 
									$(t).show().attr({ cellPadding: 0,cellSpacing: 0,border: 0 }).removeAttr('width');  
			var	p = $.extend({ height: 200, width: 800 , sortorder : 'desc',page: 1,total: 1,rp: 15,rpOptions: [1,10, 15, 20, 30, 50], title: false,query: '',qtype: '',minColToggle: 1,getGridClass: function(g) {return g;},datacol: {} }, pp);
			var g = {	hset: {},_p : p ,_pp : pp ,_data : {rows: [], page: 0, total: 0}
			,rePosDrag: function () { 	var cdleft = 0 - this.hDiv.scrollLeft , cdcounter=0;
										if (this.hDiv.scrollLeft > 0) cdleft -= Math.floor(p.cgwidth / 2);
										$(g.cDrag).css({ 	top: g.hDiv.offsetTop + 1});
										$('thead tr:first th:visible', this.hDiv).each(function () {	var n = $('thead tr:first th:visible', g.hDiv).index(this), cdpos = parseInt($('div', this).width());
																										if (isNaN(cdpos))	cdpos = 0;	
																										if (cdleft == 1)	cdleft += 1;
																										cdpos = cdpos + cdleft + (   n+2) ;
																										$('div:eq(' + n + ')', g.cDrag).css({'left': (!(browser.mozilla) ? cdpos - cdcounter : cdpos) + 'px'}).show();
																										cdleft = cdpos;
																										cdcounter++;
																										});
			}
			,fixHeight: function (newH) {	newH = $(g.bDiv).height();
											var hdHeight = $(this.hDiv).height();	$('div', this.cDrag).each( function () { 	$(this).height(newH + hdHeight);	}	);
											var nd = parseInt($(g.nDiv).height(), 10);
											if (nd > newH)								$(g.nDiv).height(newH).width(200);
											else										$(g.nDiv).height('auto').width('auto');
																						$(g.zDiv).height('auto').width(160);
											var hrH = g.bDiv.offsetTop + newH;			if (p.height != 'auto'  ) hrH = g.vDiv.offsetTop;
											$(g.rDiv).css({	height: hrH	});
			}
			,dragStart: function (dragtype, e, obj) { 
										if (dragtype == 'colresize' ){ 	$(g.nDiv).hide();  
							 											var n = $('div', this.cDrag).index(obj),ow = $('th:visible div:eq(' + n + ')', this.hDiv).width();
																		$(obj).addClass('dragging').siblings().hide();
																		$(obj).prev().addClass('dragging').show();
																		this.colresize = {startX: e.pageX,ol: parseInt(obj.style.left, 10),ow: ow,n: n};
																		$('body').css('cursor', 'col-resize');
																		$('body').noSelect();
											return;
										} 
										if (dragtype == 'vresize') {	var hgo = false;			$('body').css('cursor', 'row-resize');
																		if (obj) { 	hgo = true; 	$('body').css('cursor', 'col-resize'); 
																		}
																		this.vresize = {h: p.height,sy: e.pageY,w: p.width,sx: e.pageX,hgo: hgo};
																		$('body').noSelect();
																		return;
										} 
										if (dragtype == 'colMove') {	$(e.target).disableSelection();  
										 								$(g.nDiv).hide(); 
									 									this.hset				= $(this.hDiv).offset();
									 									this.hset.right			= this.hset.left + $('table', this.hDiv).width();
									 									this.hset.bottom		= this.hset.top + $('table', this.hDiv).height();
									 									this.dcol				= obj;
									 									this.dcoln				= $('th', this.hDiv).index(obj);
									 									this.colCopy			= document.createElement("div");
									 									this.colCopy.className	= (browser.msie) ? "colCopy ie"  : "colCopy";
									 									this.colCopy.innerHTML	= obj.innerHTML;
									 									$(this.colCopy).css({position: 'absolute','float': 'left',display: 'none',textAlign: obj.align});
									 									$('body').append(this.colCopy);
									 									$(this.cDrag).hide();
										}
										$('body').noSelect();
						},
			dragMove: function (e) {
										if (this.colresize) {			var n  = this.colresize.n, diff=	e.pageX - this.colresize.startX, nleft	=this.colresize.ol + diff, nw =	this.colresize.ow + diff;
																		if (nw > 5) { 	$('div:eq(' + this.colresize.n + ')', this.cDrag).css('left', nleft);
																						this.colresize.nw = nw;
																		} return;
										}  
										if (this.vresize) {  			var v = this.vresize, y = e.pageY, diff = y - v.sy;
																		if (!p.defwidth) p.defwidth = p.width;
																		if (p.width != 'auto' && !p.nohresize && v.hgo) {	var x = e.pageX, xdiff = x - v.sx,newW = v.w + xdiff;
																															if (newW > p.defwidth) {	this.gDiv.style.width = newW + 'px';
																																						p.width = newW;
																															}
																		}
																		var newH = v.h + diff;
																		if ((newH > 25 || p.height < 25) && !v.hgo) {	this.bDiv.style.height = newH + 'px';
																														p.height = newH;
																														this.fixHeight(newH);
																														g._p.vresizegrid(g._p.itemvnum, (1 *  newH + 60));
																		}
																		v = null; return;
										}  
										if (this.colCopy) {				$(this.dcol).addClass('thMove').removeClass('thOver');
																		if (e.pageX > this.hset.right || e.pageX < this.hset.left || e.pageY > this.hset.bottom || e.pageY < this.hset.top) 	$('body').css('cursor', 'move');
																		else  	 	$(this.colCopy).css({top: e.pageY + 10,left: e.pageX + 20,display: 'block'});
										}
			},
			verticl : function(newH){	this.tempVert = p.height ;
										this.bDiv.style.height = newH + 'px';
										p.height = newH;
										this.fixHeight(newH);
										g._p.vresizegrid(g._p.itemvnum, (1 *  newH + 60));
										
			},
			tempVert : 25
			,togVert: function(){		
										this.verticl(this.tempVert);	 

			},
			dragEnd: function () {   if (this.colresize) { var drag ='', drop=''
										var n = this.colresize.n,nw = this.colresize.nw, nw3 = Math.floor (.5 + nw/3).toString();
										$('th:visible div:eq(' + n + ')', this.hDiv).css('width', nw);
										var elemenid =  $('th:visible div:eq(' + n + ')', this.hDiv)[0].offsetParent.abbr || '';
										if (!! elemenid)
										$.sys._getpst(	'rszfld'	,{'value0' : elemenid   , 'value1' :     nw3     }		,'POST' );	
										$('tr', this.bDiv).each(	function () { 	var $tdDiv = $('td:visible div:eq(' + n + ')', this); 	$tdDiv.css('width', nw); 	});
										this.hDiv.scrollLeft = this.bDiv.scrollLeft;
										$('div:eq(' + n + ')', this.cDrag).siblings().show();
										$('.dragging', this.cDrag).removeClass('dragging');
										this.rePosDrag();
										this.fixHeight();
										this.colresize = false;
										if ($.cookies) { var name = p.colModel[n].name;	  	$.cookie('flexiwidths/'+name, nw); 	}
									} else if (this.vresize)		this.vresize = false;
									  else if (this.colCopy) {		$(this.colCopy).remove();
										if (this.dcolt !== null) {
											if (this.dcoln > this.dcolt)		$('th:eq(' + this.dcolt + ')', this.hDiv).before(this.dcol);
											else								$('th:eq(' + this.dcolt + ')', this.hDiv).after(this.dcol);
											this.switchCol(this.dcoln, this.dcolt);
											$(this.cdropleft).remove();
											$(this.cdropright).remove();
											this.rePosDrag();
											drag = this.dcoln;
											drop = this.dcolt;
										}
										this.dcol = null; this.hset = null; this.dcoln = null; this.dcolt  = null; this.colCopy = null;
										$('.thMove', this.hDiv).removeClass('thMove');
										$(this.cDrag).show();
									}
									if (!! drag && !! drop) 	$.sys._dofieldord( this._p.itemid,this._p.selectid, drag, drop);
									$('body').css('cursor', 'default');
									$('body').noSelect(false);
			},
			toggleCol: function (cid, visible) { 	var ncol = $("th[axis='col" + cid + "']", this.hDiv)[0], n = $('thead th', g.hDiv).index(ncol), cb = $('[value=' + cid + ']', g.nDiv)[0];
													if (visible == null)   		visible = ncol.hidden;
													if ($('input:checked', g.nDiv).length < p.minColToggle && !visible) 	return false;	 
													if (visible) {		ncol.hidden = false;	$(ncol).show();  	cb.checked = true;	
																								$.sys._getpst('addfld',{'value0' : ncol.abbr    },'POST' );	
																} 																				   
													else	{			ncol.hidden = true;		$(ncol).hide(); 	cb.checked = false;			   
																								$.sys._getpst('delfld',{'value0' : ncol.abbr    },'POST' );	
															}
													$('tbody tr', t).each( 	function () {	if (visible)		$('td:eq(' + n + ')', this).show();
																							else  				$('td:eq(' + n + ')', this).hide();
																		});
													this.rePosDrag();
													return visible;
			},
			switchCol: function (cdrag, cdrop) {  	$('tbody tr', t).each( function () { 	if (cdrag > cdrop)	$('td:eq(' + cdrop + ')', this).before($('td:eq(' + cdrag	+ ')', this));
																							else				$('td:eq(' + cdrop + ')', this).after($('td:eq(' + cdrag	+ ')', this));
																						});
												if (cdrag > cdrop)   											$('tr:eq(' + cdrop + ')', this.nDiv).before($('tr:eq(' + cdrag + ')', this.nDiv));
												else															$('tr:eq(' + cdrop + ')', this.nDiv).after($('tr:eq(' + cdrag + ')', this.nDiv));
 												this.hDiv.scrollLeft = this.bDiv.scrollLeft;
			}
			,addData: function (data) {			$.sys._dbhash(data);
												$('.pReload', this.pDiv).removeClass('loading');	this.loading = false;
												p.total =  data.rows[0].reccount;
												if (p.total === 0) {	$('tr, a, td, div', t).unbind();	$(t).empty();	p.pages = 1; 	p.page	= 1;
																		$('.pPageStat', this.pDiv).html('No items');	return false;
												}
												p.pages = Math.ceil(p.total / p.rp);
												p.page	= data.page;
												var r1 = (p.page - 1) * p.rp + 1, r2 = r1 + p.rp - 1;	if (p.total < r2)  r2 = p.total;		
												$('.pcontrol input'	, this.pDiv).val(p.page	);
												$('.pcontrol span'	, this.pDiv).html(p.pages	);
												$('.pPageStat'		, this.pDiv).html('Displaying '+ r1+ ' to ' + r2 + ' of ' +p.total + ' items');
												var tbody = document.createElement('tbody');  //build new body
												$.each(data.rows, function (i, row) {		var tr = document.createElement('tr'), _rec_guid	= row[row.tablenam.substr(4) + '___rec_guid'],j=0;
													if (i % 2 )	tr.className = 'erow';
													$('thead tr:first th', g.hDiv).each( 	function () { var td	=	document.createElement('td' ) , elemid =  p.colModel[j++].name	;
																							td.innerHTML	=	row[ elemid]  ;
																							td.id			=	elemid + '___' + _rec_guid;
																							$(td).attr('abbr', $(this).attr('abbr')) ;
																							$(tr).append(td);	td = null;
																						});
													$(tbody).append(tr);		tr = null;
												});
												$('tr', t).unbind();
												$(t).empty();
												$(t).append(tbody);
												$('tbody tr td', g.bDiv).each(function () {
													var tdDiv = document.createElement('div'), pth = $('th:eq(' +  $('td', $(this).parent()).index(this) + ')', g.hDiv).get(0);
													if (!! pth ) {
											 			if (p.sortname == $(pth).attr('abbr') && p.sortname) 	this.className = 'sorted';
														$(tdDiv).css({textAlign: pth.align, width: $('div:first', pth)[0].style.width});				 
														if (pth.hidden) $(this).css('display', 'none');
													} 																									 
													tdDiv.innerHTML	=	(this.innerHTML == '')	?  '&nbsp;' :	this.innerHTML;						 
								 					$(this).empty().append(tdDiv).removeAttr('width');											 
												});
												$('tbody tr', g.bDiv).on('click', function (e) { var obj	 = (e.target || e.srcElement);	if (obj.href || obj.type) 		return true;
																		if (e.ctrlKey || e.metaKey)		return;
																		$(this).toggleClass('trSelected');
																		if(!!	$.sys._stype[ $(this).context.offsetParent.firstChild.className].length)
																		$.sys._gridset[$(this).context.offsetParent.firstChild.className ].instance.grid.togVert();

																		$.sys._doDetail($(this) );
																		$(this).siblings().removeClass('trSelected'); })
													.on('mousedown', function (e) {				if (e.shiftKey) { 				$(this).toggleClass('trSelected');		g.multisel = true;		this.focus();		$(g.gDiv).noSelect();}
																								if (e.ctrlKey || e.metaKey) {	$(this).toggleClass('trSelected');		g.multisel = true;		this.focus();	}})
													.on('mouseup' ,	function (e)	{			if (g.multisel && ! (e.ctrlKey || e.metaKey)) {		g.multisel = false;		$(g.gDiv).noSelect(false);	}})
													.hover(			function (e)	{			if (g.multisel && e.shiftKey) $(this).toggleClass('trSelected');}, function () {});
												this.rePosDrag();	tbody = null; 
												if   (data.rows.length >= p.total) 
												 g._data = data
												else data = null;
												this.hDiv.scrollLeft = this.bDiv.scrollLeft;
			 },
			changeSort: function (th) { if (this.loading)  return true;
										$(g.nDiv).hide();
										if (p.sortname == $(th).attr('abbr')) 		 p.sortorder = (p.sortorder == 'asc')  ? 'desc' : 'asc'
										$(th).addClass('sorted').siblings().removeClass('sorted');
										$('.sdesc'	, this.hDiv).removeClass('sdesc');
										$('.sasc'	, this.hDiv).removeClass('sasc');
										$('div'		, th).addClass('s' + p.sortorder);
										p.sortname	= $(th).attr('abbr');
										if (!!! g._data.rows.length  )	{		p.onSubmit(this );	return;}
										$.sys._sortOn(	g._data.rows, g._p.sortname, (g._p.sortorder== 'asc')  	? false : true, false); 
										var copydata = g._data;
										this.addData(copydata );
			},
			populate : function (xtra) {	return  (this.loading)   ?  true :   p.onSubmit(this,xtra );	
			},
			doSearch: function () {		p.newp = 1;
										p.query = $('[name=q]'			, g.sDiv).val();	//query  abc* (textstring)
										p.qtype = $('select[name=qtype]', g.sDiv).val();	//qtype =no tbl_   tablename___fieldname
								 		if (!!! this.loading)	
										p.onSubmit(this );
			},
			changePage: function (ctype) {	if (this.loading)  return true;
										switch (ctype) {
											case 'first': 							p.newp = 1; 							break;
											case 'prev':	if (p.page > 1)   		p.newp = parseInt(p.page, 10) - 1;		break;
											case 'next': 	if (p.page < p.pages) 	p.newp = parseInt(p.page, 10) + 1;		break;
											case 'last': 							p.newp = p.pages; 						break;
											case 'input':	var nv = parseInt($('.pcontrol input', this.pDiv).val(), 10);
																 if (isNaN(nv))  		nv = 1;
																 if (nv < 1)  			nv = 1;
															else if (nv > p.pages)   	nv = p.pages;
																 $('.pcontrol input', this.pDiv).val(nv);	p.newp = nv;	break;
										}
										if (p.newp == p.page)	return false;
										p.onSubmit(this );	
			},
			setupHead: function (p ){ 	if (!!! p.colModel)  return;
										var	thead = document.createElement('thead'),	 tr = document.createElement('tr');
										for (var i = 0; i < p.colModel.length; i++) {var th = document.createElement('th');
											$(th).attr('axis', 'col' + i);  
											th.innerHTML =  p.colModel[i].display	|| "";		
											$(th).attr('width',p.colModel[i].width	||  30);
											$(th).attr('abbr', p.colModel[i].name);
											th.align	= p.colModel[i].align		|| '';
											th.hidden	= (!! p.colModel[i].hidden)	||  (!! $(p.colModel[i]).attr('hide'))	;
											th.process	= p.colModel[i].process		|| '';
											$(tr).append(th); 
										}
										$(thead).append(tr);
										$(t).prepend(thead);
			},
			 pager  : 0
			,gDiv	: document.createElement('div')
			,hDiv	: document.createElement('div')	   //create header container
			,bDiv	: document.createElement('div')	   //create body container
			,vDiv	: document.createElement('div')	   //create grip
			,rDiv	: document.createElement('div')	   //create horizontal resizer
			,cDrag	: document.createElement('div')	   //create column drag
			,nDiv	: document.createElement('div')	   //create column show/hide popup
			,zDiv	: document.createElement('div')	
			,tDiv	: document.createElement('div')	   //create toolbar
			,sDiv	: document.createElement('div')
			,pDiv	: document.createElement('div')
			,hTable : document.createElement('table')
	};
        g = p.getGridClass(g);  
		g.setupHead(p);
		g.gDiv.className = 'dtmgrid';	if (p.width != 'auto')   g.gDiv.style.width = p.width + (isNaN(p.width) ? '' : 'px');		if (browser.msie)    $(g.gDiv).addClass('ie');	 //create global container
		g.hDiv.className = 'hDiv';			$(g.hDiv).append('<div class="hDivBox"></div>');													
		g.bDiv.className = 'bDiv';																												
		g.vDiv.className = 'vGrip';		$(g.vDiv).mousedown(function (e) {	g.dragStart('vresize', e);}).html('<span></span>');					
		g.rDiv.className = 'hGrip';
 		g.pDiv.className = 'pDiv';		g.pDiv.innerHTML = '<div class="pDiv2"></div>';	 //create pager container
		g.hTable.cellPadding = 0;		g.hTable.cellSpacing = 0;     
		$(t).before(g.gDiv);
		$(g.gDiv).append(t);
		$(t).before(g.hDiv);
		$('div', g.hDiv).append(g.hTable);
		var thead = $("thead:first", t)[0]; if (thead) $(g.hTable).append(thead); thead = null;
		$('thead tr:first th', g.hDiv).each(function () {	var thdiv = document.createElement('div');
							if ($(this).attr('abbr')) {
								$(this).click(function (e) { 	var obj = (e.target || e.srcElement); 	if (obj.href || obj.type) 
																return true;
																g.changeSort(this);
								});
								if ($(this).attr('abbr') == p.sortname) {	this.className = 'sorted';
																			thdiv.className = 's' + p.sortorder;
								}
							}
							if (this.hidden)   		$(this).hide();
					 		if (!!! this.width )	this.width  = 60; if (this.width < 20)	this.width  = 20;
							$(thdiv).css({textAlign: this.align,width: this.width + 'px'});
							thdiv.innerHTML			= this.innerHTML;
							$(this).empty().append(thdiv).removeAttr('width').mousedown(function (e) {	g.dragStart('colMove', e, this);})
							.hover(function () {		if (!g.colresize && !$(this).hasClass('thMove') && !g.colCopy)  		$(this).addClass('thOver');
														if ($(this).attr('abbr') != p.sortname && !g.colCopy && !g.colresize && $(this).attr('abbr')) 
																$('div', this).addClass('s' + p.sortorder);
														else if ($(this).attr('abbr') == p.sortname && !g.colCopy && !g.colresize && $(this).attr('abbr')) {
																var no = (p.sortorder == 'asc') ? 'desc' : 'asc';
																$('div', this).removeClass('s' + p.sortorder).addClass('s' + no);
														}
														if (g.colCopy) { 			var n = $('th', g.hDiv).index(this);
																					if (n == g.dcoln)  		return false;
																					if (n < g.dcoln)	$(this).append(g.cdropleft);
																					else  				$(this).append(g.cdropright);
																					g.dcolt = n;
														} else if (!g.colresize) { 
																					if ($(this).hasClass('sorted'))		$(g.nBtn).addClass('srtd');
																					else   								$(g.nBtn).removeClass('srtd');
														}
								}, function () { 		$(this).removeClass('thOver');
														if ($(this).attr('abbr') != p.sortname) 	$('div', this).removeClass('s' + p.sortorder);
												else	if ($(this).attr('abbr') == p.sortname) { 	var no = (p.sortorder == 'asc') ? 'desc' : 'asc';
																									$('div', this).addClass('s' + p.sortorder).removeClass('s' + no);
														}
														if (g.colCopy) {	$(g.cdropleft).remove();	$(g.cdropright).remove();	g.dcolt = null;	}
														});  
						});
		$(t).before(g.bDiv);
		$(g.bDiv).css({ height: (p.height == 'auto') ? 'auto' : p.height + "px"	}).scroll(function (e) {	g.hDiv.scrollLeft = g.bDiv.scrollLeft;	g.rePosDrag(); 	}).append(t);
		if (p.height == 'auto') 	$('table', g.bDiv).addClass('autoht');
        var cdcol = $('thead tr:first th:first', g.hDiv)[0];
        if(cdcol !== null) {
            g.cDrag.className = 'cDrag';
            $(g.bDiv).before(g.cDrag);
            var cdheight = $(g.bDiv).height(), hdheight = $(g.hDiv).height();
            $(g.cDrag).css({ top: -hdheight + 'px'});
            $('thead tr:first th', g.hDiv).each(function() {         var cgDiv = document.createElement('div');		
																	$(g.cDrag).append(cgDiv);
																	if (!p.cgwidth)      p.cgwidth = $(cgDiv).width() ;
																	$(cgDiv).css({ height: cdheight + hdheight }).mousedown(function(e) {   g.dragStart('colresize', e, this);    });
 															 });
        }
		$('tbody tr:odd', g.bDiv).addClass('erow');
		$(g.bDiv).after(g.vDiv);
		$(g.rDiv).mousedown(function (e) { g.dragStart('vresize', e, true); }).html('<span></span>').css('height', $(g.gDiv).height());
		$(g.gDiv).append(g.rDiv);
		$(g.bDiv).after(g.pDiv);	
 		$('div'				, g.pDiv).html('<div class="pGroup">'			 +  '<div class="nBtn   pButton"><span></span></div>' +  '<div class="pFirst pButton"><span></span></div>'+  '<div class="pPrev  pButton"><span></span></div>'+  '</div>'
										+  '<div class="btnseparator"></div>'+  '<div class="pGroup"><span class="pcontrol">Page  <input type="text" size="4" value="1" /> of <span> 1 </span></span></div>'
										+  '<div class="btnseparator"></div>'+  '<div class="pGroup"> <div class="pNext pButton"><span></span></div>' +  '<div class="pLast pButton"><span></span></div> </div>'
										+  '<div class="btnseparator"></div>'+  '<div class="pGroup"> <div class="pReload pButton"><span></span></div> </div>'
										+  '<div class="btnseparator"></div>'+  '<div class="pGroup"><span class="pPageStat"></span></div>');
		$('.nBtn'			, g.pDiv).click(function () {		 $(g.nDiv).css('left',0).toggle(); return true; 	});
		$('.pReload'		, g.pDiv).click(function () {							g.populate();					});
		$('.pFirst'			, g.pDiv).click(function () {							g.changePage('first');			});
		$('.pPrev'			, g.pDiv).click(function () {							g.changePage('prev');			});
		$('.pNext'			, g.pDiv).click(function () {							g.changePage('next');			});
		$('.pLast'			, g.pDiv).click(function () {							g.changePage('last');			});
		$('.pcontrol input'	, g.pDiv).keydown(function (e) { if (e.keyCode == 13)	g.changePage('input');			});
		var opt = '',sel = '';
		for (var nx = 0; nx < p.rpOptions.length; nx++) { sel = '';  if (p.rp == p.rpOptions[nx]) sel = 'selected="selected"';
				opt += "<option value='" + p.rpOptions[nx] + "' " + sel + " >" + p.rpOptions[nx] + "&nbsp;&nbsp;</option>";
		}
		$('.pDiv2', g.pDiv).prepend("<div class='pGroup'><select name='rp'>" + opt + "</select></div> <div class='btnseparator'></div>");
		$('select', g.pDiv).change(function () {	if (p.onRpChange) p.onRpChange(+this.value);
														else {  p.newp = 1;	p.rp = +this.value;
																g.populate();
				}
			});
		if (p.searchitems && p.searchitems.length > 0) { 
			p.srchoptions = (p.itemid == "LabResults___LabResults")  ?  "<div  class='pGroup'><select id='" + p.itemid + "___searchnm' name='Searches' class='savedsearch' ><option value='all'>all&nbsp;&nbsp;</option></select></div><div class='pSearch2 pButton'><span></span></div><div class='pSearchd pButton'><span></span></div> </div>"  : '';
			$('.pDiv2', g.pDiv).prepend("<div class='pGroup'> <div class='pSearch pButton'><span></span></div>" + p.srchoptions + "<div class='btnseparator'></div>");
			$('.pSearch', g.pDiv).click(function () {		$(g.sDiv).slideToggle('fast', function () { $('.sDiv:visible input:first', g.gDiv).trigger('focus'); });  
 															 });
	 		if (p.itemid == "LabResults___LabResults"){
								$('.pSearchd', g.pDiv).click(function (e) { $.sys._clearselist(); 	$.sys._gridset["LabResults___LabResults"].instance.grid.populate();
																										});	 
								g.zDiv.className = 'zDiv'; 	g.zDiv.innerHTML =	'<table cellpadding="0" cellspacing="0"><tbody>'
																			+ '<tr><td id="menu___lab_name" class="ndcolz" >By Lab</td></tr>'
																			+ '<tr><td id="menu___collname" class="ndcolz" >By Collector</td></tr>'
																			+ '<tr><td id="menu___clientnm" class="ndcolz" >By Client</td></tr>'
																			+ '<tr><td id="menu___lastname" class="ndcolz" >By Last Name</td></tr>'
																			+ '<tr><td id="menu___firstnam" class="ndcolz" >By First Name</td></tr>'
																			+ '<tr><td id="menu___ssn_numb" class="ndcolz" >By SSN</td></tr>'
																			+ '<tr><td id="menu___otherone" class="ndcolz" >Other</td></tr>'
																			+ '<tr><td id="menu___nosearch" class="ndcolz" >Reset Search</td></tr>'
																			+ '</tbody></table>	'	;
								$(g.zDiv).css({marginBottom: ($(g.bDiv).height() * -1),display: 'none',top: g.bDiv.offsetTop}).noSelect();
								$(g.gDiv).prepend(g.zDiv);
								$('.pSearch2', g.pDiv).click(function () {		 $(g.zDiv).css('left',0).toggle(); return true; 	});	
			}
			g.sDiv.className = 'sDiv';
			var sitems = p.searchitems,sopt = '', sel = '';
			for (var s = 0; s < sitems.length; s++) {	sel = '';
				if (p.qtype === '' &&  sitems[s].isdefault === true) {	p.qtype =  sitems[s].name;	sel = 'selected="selected"';	}  
					 sitems[s].opthtm = "<option value='" + sitems[s].name + "' " + sel + " >"  + sitems[s].display + "&nbsp;&nbsp;</option>";
					 	sopt += sitems[s].opthtm;
			}
			if (p.qtype === '') p.qtype = sitems[0].name;
			$(g.sDiv).append('<div class="sDiv2">Find <input type="text" value="' + p.query + '" size="30" name="q" class="qsbox" />  <select  id ="' + p.selectid + '"  name="qtype">' + sopt + '</select></div>'); 
			$('[name=q]'			, g.sDiv).keydown(function (e) {	if (e.keyCode == 13)								g.doSearch();	});
			$('select[name=qtype]'	, g.sDiv).keydown(function (e) {	if (e.keyCode == 13)  								g.doSearch();	});
			$('[value=Clear]'		, g.sDiv).click(function () {		$('[name=q]', g.sDiv).val('');		p.query = '';	g.doSearch();	});
			$(g.bDiv).after(g.sDiv);
		}
		$(g.pDiv, g.sDiv).append("<div style='clear:both'></div>");
		g.cdropleft				=	document.createElement('span');	g.cdropleft.className	=	'cdropleft';
		g.cdropright			=	document.createElement('span');	g.cdropright.className	=	'cdropright';
		if ($('th', g.hDiv).length) {
			g.nDiv.className = 'nDiv'; 	g.nDiv.innerHTML = "<table cellpadding='0' cellspacing='0'><tbody></tbody></table>";
			$(g.nDiv).css({marginBottom: ($(g.bDiv).height() * -1),display: 'none',top: g.bDiv.offsetTop}).noSelect();
			var cn = 0;
			$('th div', g.hDiv).each(function () { 	
					var kcol = $("th[axis='col" + cn + "']", g.hDiv)[0], chk =  (kcol.style.display == 'none') ? '' : 'checked="checked"';
					$('tbody', g.nDiv).append('<tr><td class="ndcol1"><input type="checkbox" ' + chk + ' class="togCol" value="' + cn + '" /></td><td class="ndcol2">' + this.innerHTML + '</td></tr>');
					cn++;
			});
			$('td.ndcol2', g.nDiv).click(function () {		if ($('input:checked', g.nDiv).length <= p.minColToggle && $(this).prev().find('input')[0].checked) return false;
															return	g.toggleCol($(this).prev().find('input').val());
			});
			$('input.togCol', g.nDiv).click(function () {	if ($('input:checked', g.nDiv).length < p.minColToggle && this.checked === false) return false;
															$(this).parent().next().trigger('click');
			});
			$(g.gDiv).prepend(g.nDiv);
		}
		$(g.bDiv).hover(function () {	$(g.nDiv).hide();	 }, function () {	if (g.multisel) {		g.multisel = false;	}	});
		$(g.gDiv).hover(function () {}, function () {	$(g.nDiv).hide();  });
		$(g.zDiv).hover(function () {}, function () {	$(g.zDiv).hide();  });
		$(document).mousemove(function (e) {	g.dragMove(e);	}).mouseup(function (e) {	g.dragEnd(); }).hover(function () {}, function () {	g.dragEnd();	});
  		g.rePosDrag();
		g.fixHeight();
		t.p	   = p;
		t.grid = g;
		return t;
	};
	var docloaded = false;
	$(document).ready(function () {	docloaded = true;	});
	$.fn.dtmgrid		= function (p) {				return this.each(function () { 	if ( docloaded) { $.addFlex(this, p); return; }	$(this).hide();	
																						var t = this;	
																						$(document).ready(function () {$.addFlex(t, p);	});	});
	}; 
	$.fn.noSelect		= function (p) {				if  (p === null)  	
																			return	 this.each(function () { 
																								if (browser.msie || browser.safari) $(this).bind('selectstart', function () {	return false;	});
											 													if (browser.mozilla) {				$(this).css('MozUserSelect', 'none');
																																	$('body').trigger('focus');
																								} else								$(this).attr('unselectable'	, 'on');
																								}) 
																			 return	  this.each(function () { 			
																								if (browser.msie || browser.safari) $(this).unbind('selectstart');
																						else	if (browser.mozilla)				$(this).css('MozUserSelect', 'inherit');
																						else										$(this).removeAttr('unselectable', 'on');
																								});
	};
	$.fn.accord = function(options) { return this.each(function() {		
											$('#acc__base'	 ).find('a').addClass('h');
										if ($('#acc__base .h').next('section:not(.outer)').length) 		
											$('#acc__base .h').next('section:not(.outer)').wrap('<section class="outer"> </section>'); 
											$('#acc__base .h').each(function(){	var $this = $(this); if ( ! $this.parent('section.new').length)  	
																				$this.add( $this.next('section.outer') ).wrapAll('<section class="new" ></section>'); 
											});
											$('#acc__base .h').each(function(){ var $node = $(this), xanode = $node.id || $node[0].id;
																			if ($node.find('section').length ||	 $node.next('section').length){	var txt = []; 
																				if ($node.find('> a').length){   $node.find('> a').addClass("trigger"); return; }
																				$node.each(function(){  $.each(this.childNodes, function() {  	if (this.nodeType == 3 && $.trim(this.nodeValue)) { txt.push(this);} })});
																				$(txt).wrap( '<a id="s' +	xanode + '" class="trigger" ></a>'   )
																				return;
																			}
																			$node.addClass('last-child');
																			if (  $node.find('> a').length)  $node.find('> a').addClass("trigger"); 
											});
											$('#acc__base .h a.trigger').closest('section').find('> section').not('.shown').hide().closest('section').find('a.open').removeClass('open').data('state', 0);
											$('#acc__base').delegate('a.trigger',  'click', function(ev) { 
															var $thislink = $(this) ,$nextEl= $thislink.closest('section').find('> section') ;
															if (($nextEl).length && $thislink.data('state') ) {
																	$thislink.removeClass('open');
																	$nextEl.filter(':visible')['slideUp'](10, function() {$thislink.data('state', 0);});
															}
															if (($nextEl.length && !$thislink.data('state')) || (!($nextEl).length && $thislink.closest('section').not('.shown'))) {
																	$thislink.addClass('open');
																	$nextEl.filter(':hidden')['slideDown'](10, function() {$thislink.data('state', 1);});
															}
															return  true;
														});});
}; 
})(jQuery);
 $(function(){
 $.sys = {
		_canModify	: 'user',_datime    : new Date() ,_expired	: 900000	,_username	: undefined	,_diaTime	: undefined  ,_optdatalist		: [] 
		,_outline	: {}	,_cookash	: {}		 ,_db		: {}		,_taxonomy	: {}		,_gridset	: {} 
		,_gridrep	: {}	,_hotEl		: {}		 ,_activrecs : {}		,_tblaction : {}		,_rtype		: {} 
		,_stype		: {}	,_tablex	: {}		 ,_dlg		: {}		,_itemz		: { "add"	:	{name: "Add Field"		, icon: "add"	    },"edit" :	{name: "Edit Field"		, icon: "edit"		},"delete"	:	{name: "Delete Field"	, icon: "delete"	},"search"	:	{name: "Search"			, icon: "magnifier"	},"usmode"	:	{name: "Modes"	 }}	
		,_reports	: function (rp ){		var	 RPT = {},acga =[], accset = {},hsfary =	[ 'headerer','nrmlsect','footerer','dogrider' ]  ;
											if (!!! rp || !!! rp[0] || !!! rp[0].rec_guid)	return false;
											$('<div id="reporter"> <section id="acc__base" class="accord"></section> </div>'  ).appendTo("body" ).dialog({height:1000,width:900, modal: true ,autoOpen: false});
											for (var i =0; i < rp.length; i++) {
													$.sys._outline[rp[i].mainview]										=	$.sys._outline[rp[i].mainview ]										|| {};		 
													$.sys._outline[rp[i].mainview][rp[i].sub_view ]						=	$.sys._outline[rp[i].mainview ][rp[i].sub_view]						|| {};
				  									$.sys._outline[rp[i].mainview][rp[i].sub_view ][ rp[i].initfunc]	=	$.sys._outline[rp[i].mainview ][rp[i].sub_view][ rp[i].initfunc]	|| [];
													$.sys._outline[rp[i].mainview][rp[i].sub_view ][ rp[i].initfunc].push( rp[i]  ) ;
				 									if ( rp[i].fieldtyp == 'gfld' ||  rp[i].fieldtyp.indexOf('grid') >=0 ) 	acga.push(rp[i]); 
 											 }
											for (var acc in  $.sys._outline) for (var subacc in  $.sys._outline[acc])  for (var hf=0; hf< hsfary.length; hf ++ ){	
													if(!!!  $.sys._outline[acc][subacc][hsfary[hf]] ) 
													continue;
													var   ahtml = $.sys._outline[acc][subacc][hsfary[hf]],  adn = ahtml[0].itemvnum.split('-'), str='',texstr='';
													for(i = 0; i< adn.length; i++){ 
														for (j = 1; j <= adn[i]; j++){
																if (!!! $('#acc__base'	+ str +  '-' + j	).length) 							
 																		$('#acc__base'	+ str				).append(	
															'<a  id="h'					+ str +  '-' +  j  + '" class="unused ' +  '" >' +  texstr +  j  + '  </a>'
															+ '<section id="acc__base'	+ str +  '-' +  j  + '"	class="inner ">'
															+ '<div id="p'				+ str +  '-' +  j  + '"	class=" unused  "	  >'
															+ '<div id="pheaderer'		+ str +  '-' +  j  + '"	class=" headerer ui-widget-header"	  ></div>'
															+ '<div id="pnrmlsect'		+ str +  '-' +  j  + '"	class=" nrmlsect ui-widget-header"	  ></div>'
															+ '<div id="pfooterer'		+ str +  '-' +  j  + '"	class=" footerer ui-widget-header"	  ></div>'
															+'</div>'					
															+ '</section>');							}							
														$('#h' + str).removeClass('unused');
														str += '-' + adn[i];
														texstr +=	adn[i] + '.' ;
													}
													$('#h-'  +  ahtml[0].itemvnum).removeClass('unused');
 													if ( hsfary[hf]  == 'dogrider') continue;
														for(var ii =0; ii < ahtml.length; ii++){ 	var   spaces		= ''; 	for (var ti=0; ti< ahtml[ii].size_xpx/9  ; ti++ )  spaces += 'O' ;
																$('#p-'		+  ahtml[ii].itemvnum		).removeClass('unused')
																$('#p'		+  ahtml[ii].initfunc   + '-' 	+	 ahtml[ii].itemvnum								 ).append('<div  class="  ui-widget-content " data-rec_guid="'  + ahtml[ii].rec_guid +'" id="rx-'	+  ahtml[ii].initfunc	+ '-'	+    ahtml[ii].itemvnum + '-'  + ahtml[ii].elemenid  + '" >' +  spaces + '</div>');
																$('#rx-'	+  ahtml[ii].initfunc	+ '-'	+    ahtml[ii].itemvnum + '-'  + ahtml[ii].elemenid  ).css({  display : 'block',  position : 'absolute' ,     top :   ahtml[ii].rowvalue  + "px", left : ahtml[ii].colvalue + "px", width:  ahtml[ii].size_xpx  + "px", height :   "14px", border : "1px solid" });	 	 
														}
												}
												for (var i =0; i < acga.length; i++){
														if (acga[i]['fieldtyp'].indexOf('grid') < 0) 	continue;
														$.sys._rtype	[ acga[i]['modifier']	] = [];
 														$.sys._gridset[   acga[i]['modifier']	] =  { itemid :  acga[i]['modifier'], title : acga[i]['el_title'],colModel :	[], searchitems	: [], sortname : acga[i]['elemenid'].split('___')[1], url : "populate.php" , onSubmit : $.sys._populate ,usepager	: true  ,sortorder	:	"desc"  ,selectid  : 'selid___' +  acga[i]['modifier'] , itemvnum : acga[i].itemvnum , vresizegrid : $.sys._vresizegrid };
														$.sys._gridrep[   acga[i]['modifier']	] =	 $.sys._gridset[   acga[i]['modifier'] ]  ;	
														$.sys._tblaction[ acga[i]['modifier']	] =  acga[i]['fieldnam'];
														$.sys._tblaction[ acga[i]['fieldnam']	] =  acga[i]['tablenam'];
														$( '<div id= "' + acga[i]['modifier'] + '" class="' + acga[i]['modifier'] + '"> </div>' ).appendTo(  "#pnrmlsect-" + acga[i].itemvnum   );
														$('#p-'			+ acga[i].itemvnum 	).removeClass('unused')
												}
												for (var i =0; i < acga.length; i++){ 	 
														if (acga[i]['fieldtyp'] == 'gfld') {
															$.sys._gridset[  acga[i]['mainview'] + '___' +   acga[i]['sub_view'] ] ['colModel'	  ].push({'name' : acga[i]['elemenid'] , 'display' : acga[i]['el_label'] ,'hidden'    : (acga[i]['modifier'] == 'hidden') ? true : false  ,'sortable' : true, width : ( acga[i]['sizechar'] * 4  )	, fieldord	: acga[i].fieldord, rec_guid :  acga[i].rec_guid  });
															$.sys._gridset[  acga[i]['mainview'] + '___' +   acga[i]['sub_view'] ] ['searchitems' ].push({'name' : acga[i]['elemenid'] , 'display' : acga[i]['el_label'] ,'isdefault' : (acga[i]['modifier'] == 'defalt' ) ? true : false															, fieldord   : acga[i].fieldord, rec_guid :  acga[i].rec_guid  });
														}
														if (	acga[i]['fieldtyp'].indexOf('grid') < 0 ) continue;
														if (!!! acga[i]['fieldtyp'].split('___')[1]	  ) continue;
														$.sys._rtype[acga[i]['modifier'].split('___')[0] + '___' + 	acga[i]['modifier'].split('___')[0]].push({ actionid  :    "d" +  acga[i]['fieldnam'].substr(1),tablenam  :	 acga[i]['tablenam'],pagenumb  :	"1" ,rowspage  :	"15" ,sortfild  :	 acga[i]['elemenid'].split('___')[1],sortordr  :	"asc",qufield0  :	 acga[i]['fieldtyp'].split('___')[1],quwhere0  :	"has_in",quvalue0  :	""   });
												}
 	  											for (var each in $.sys._gridrep) { 
														$.sys._sortOn($.sys._gridrep[each].colModel   , 'fieldord',   false  , true); 
														$.sys._sortOn($.sys._gridrep[each].searchitems, 'fieldord',   false  , true); 
														$.sys._gridrep[each].instance = $("#" +  each).dtmgrid($.sys._gridrep[each])[0];
													//	$.sys._gridrep[each].instance.grid.populate();
												}	
												$("#acc__base" ).accord( );	
												$( "[id^=rx-]" ).draggable().on(	{ mouseup :function( event  ) {    if ($.sys._canModify != 'user' )   	
																						 $.sys._getpst(	'movfld'		,{'value0' : $(this).attr('data-rec_guid')    , 'value1' : this.style.top.split('px')[0],  'value2'  : this.style.left.split('px')[0]  }		,'POST' );	
																																}	});	 
												$( "[id^=pppp]" ).droppable(		{	drop: function( event  ) { }	});
												$("#reporter").css('z-index' ,'99' ).dialog( "open" ) ;
		}
		, _doreports	 : function(){					setTimeout(function() { $('#dvLoading').show(); }, 20);
														setTimeout(function() {	$.sys._getpst("reporter",{username :  $.sys._username   },'GET',function (data) {	
																   				$.sys._reports( (typeof (data) == "string") ? eval ('('+  data + ')' ) : data);
																				$('#dvLoading').fadeOut(2000);  	}); } 	, 50);
		} 
		,_init			: function(options)  {			if (!window.name) window.name = Math.random();  
														var cookies		=	document.cookie.split(';'); 
														for (var i in cookies) { var kv = cookies[i].split('=');
															if ((new RegExp('__session:' + window.name + '.+')).test(kv[0]) && kv[1]) 	this._cookash[kv[0].split(':', 3)[2]] = kv[1];
														}
														var matches			=	(new RegExp('__session:' + window.name + "=([^;]+);")).exec(document.cookie);
														if (matches && document.location.protocol !== matches[1]) {
															try {	window.sessionStorage.clear();	} catch (e) { for (var i in window.sessionStorage)   window.sessionStorage.removeItem(i); }
															for (var key in this._cookash)			try { window.sessionStorage.setItem(key, this._cookash[key]);       } catch (e) {};
														}
														document.cookie = '__session:' + window.name + "=" + document.location.protocol + ';path=/;expires=' + (new Date((new Date).getTime() + 120000)).toUTCString();
        }
        ,set				: function(key, val,oneX){  try {   window.sessionStorage.setItem(key, val);	  } catch (e) {} 
														document.cookie			=  (!!! oneX)	?  '__session:' + window.name + key + "=" + val + "; path=/"
																								:  '__session:' + window.name + key + "=" + val + "; path=/; expires=" + (new Date(Date.now() + 120000)).toUTCString()
														this._cookash[key]	= val;
														return this;
        }
		,'delete'			: function(key){			try {    window.sessionStorage.removeItem(key);   } catch (e) {};   document.cookie = '__session:' + window.name + key + '=; path=/; expires=Thu, 01 Jan 1970 00:00:01 GMT;';	delete this._cookash[key];	return this;
        }
		,_sortOn		:  function (arr, prop, reverse, numeric) {			if (!!! prop || !!!arr)		return arr ;
														var sort_by = function (field, rev, primer) {	return function (a, b) {	a = primer(a[field]), b = primer(b[field]);		// Reset a, b to the field
																										return						((a < b) ? -1 : ((a > b) ? 1 : 0)) * (rev ? -1 : 1);			}
														 }
														if (numeric)    arr.sort(sort_by(prop, reverse, function (a) {		return parseFloat(String(a).replace(/[^0-9.-]+/g, ''));		}));
														else			arr.sort(sort_by(prop, reverse, function (a) {		return String(a).toUpperCase();								}));
		}
		,_dbhash	: function (data ){				    data	= $.extend({rows: [], page: 0, total: 0}, data); if (data.rows.length ==0 ) return;
														var recguidid ='';
														for (var each in data.rows[0]){	if ( each.indexOf('rec_guid') <= 0)  continue;
															recguidid  = each;		//each record in a row will have this format :  tbl_name___rec_guid
															break;
														}	// setting $.sys._db[ xxxxxxxx-xxxx-xxxx-...] = object that points to data and the index into data row
														for (var i =0; i < data.rows.length; i++) $.sys._db[data.rows[i][recguidid]] = { data : data ,index : i };
		}
		,_dbhashadd	: function (recguid,newguid){		var recs =	$.sys._db[recguid], len =   recs.data.rows.length;
														recs.data.rows.push({}); 
														for (var each in  recs.data.rows[0]){  recs.data.rows[len][each] = ''
															  if ( each.indexOf('rec_guid')<= 0) continue;
															   recs.data.rows[len][each] = newguid;
															   recs.data.total++;
															   $.sys._db[newguid] = { data : recs.data ,index : len};
															   return;
														}
		}
		, _dofieldord : function( this_itemid,this_selid, cdrag,cdrop){
														if (cdrag > cdrop) cdrop--;
														if (cdrop < 1){		$.sys._gridset[this_itemid].colModel[cdrag].fieldord	= '10';
																			$.sys._gridset[this_itemid].searchitems[cdrag].fieldord	= '10';
														}
														else{				$.sys._gridset[this_itemid].colModel[cdrag].fieldord	= ( 1 * $.sys._gridset[this_itemid].colModel[cdrop].fieldord) + 1;
																			$.sys._gridset[this_itemid].searchitems[cdrag].fieldord	=		$.sys._gridset[this_itemid].colModel[cdrag].fieldord
														}
														var sopts = '';
														$.sys._sortOn(		$.sys._gridset[this_itemid].colModel   , 'fieldord',   false  , true); 
														$.sys._sortOn(		$.sys._gridset[this_itemid].searchitems, 'fieldord',   false  , true); 
														for (var i =0; i <	$.sys._gridset[this_itemid].searchitems.length; i++) 
														sopts += $.sys._gridset[this_itemid].searchitems[i].opthtm;
														$("#" + this_selid).html(sopts);
													 	$.sys._getpst ('ordfld',{ actionid : 'ordfld', tablenam : 'tbl_fieldstyle', fieldnam : 'fieldord', wherefld : 'rec_guid',whereval : $.sys._gridset[this_itemid].colModel[cdrag].rec_guid , newvalue : $.sys._gridset[this_itemid].colModel[cdrag].fieldord  } ,'POST' );
		}
		,_setDTMApp		: function(io) {				var		DTM	= {},ga =[],grid=[], tset = {},thtm =	{ count : 0 }  ,tmh = '<div id="tabs" ><ul id="tabs-list" >';
													 	if (!!! io || !!! io[0] || !!! io[0].rec_guid) return false;
														for (var i =0; i < io.length; i++) {
																var		 tabnum		= ( ~~io[i].itemvnum   <  10) ?   ~~io[i].itemvnum    :  ~~( io[i].itemvnum/10 ) 
																		,stringtab  = tabnum.toString() 
																		,subtabnum	= ( ~~io[i].itemvnum   <  10) ?   0	  :  ~~io[i].itemvnum  % 10 ;
																if (!!! thtm[stringtab]){	thtm.count++;
																		thtm[stringtab] = {  subtabs :  { count : 0, sequ : {}, list : {} },  list :	'<li id="tablist___'	+ io[i].tablenam   +'___'   + io[i].mainview  +'___'   + io[i].sub_view  +'___' + i  +'"   class="tablist___'+ io[i].tablenam   +'___'   + io[i].mainview +'___' + i + '"  ><a href="#tabs-' + stringtab +'">' + io[i].mainview +  '</a></li>\n' };
																}
																if (!!! thtm[stringtab].subtabs.list[io[i].itemvnum]) { thtm[stringtab].subtabs.count++;
																		thtm[stringtab].subtabs.list[io[i].itemvnum] =									'<li id="tablist___'								+ io[i].tablenam  +'___' + io[i].mainview  +'___'   + io[i].sub_view   +'___' + i  +'"   class="tablist___'+ io[i].tablenam  +'___'  +  io[i].sub_view  +'___' + i	+   '"	><a href="#subtabs-' + io[i].itemvnum + '" >' +  io[i].sub_view + '</a></li>\n';
																		thtm[stringtab].subtabs.sequ[subtabnum.toString()]	 =	io[i].itemvnum;
																}
																DTM[io[i]["mainview"]]						= DTM[io[i]["mainview"]]					||  {};
																DTM[io[i]["mainview"]][io[i]["sub_view"]]	= DTM[io[i]["mainview"]][io[i]["sub_view"]] ||  {"itemvnum" :  io[i].itemvnum  ,"JSONOB" : []   };
													 			if ( io[i].initfunc.indexOf("tbl_") >=0)  {	
																	if (!!! $.sys._dlg[io[i].tablenam]) $.sys._dlg[io[i].tablenam] = { dlg :  [], parentbl :  io[i].initfunc.split("___")[0], forkey : io[i].initfunc.split("___")[1]} ;
																	 $.sys._dlg[io[i].tablenam].dlg.push(io[i]); 
																	continue;
																}
																// setting up active record defs for insert update and delete of data 
																tset[io[i].tablenam]				 =  ( tset[io[i].tablenam]  ) || {};
															 	$.sys._activrecs[   io[i].tablenam] =  $.sys._activrecs[  io[i].tablenam ] || { mainview : io[i].mainview, sub_view : io[i].sub_view, tablenam :  io[i].tablenam,  rec_guid : '' , activrec : {}, fieldnames : []	};
																tset[io[i].tablenam][io[i].fieldnam] =  { optionval : 	' <option value="'  + io[i].fieldnam +  '" >' + io[i].fieldnam + '</options> ' , fieldsval	: io[i] , selectval : '"' + io[i].fieldnam + '"	:	{name: "' + io[i].fieldnam  + '" } ' } ;
																// breaking into grid fields, grids or other input types
																if ( io[i].fieldtyp == 'gfld')	{ 	ga.push(io[i]);
																									$.sys._activrecs[  io[i].tablenam ].fieldnames.push(io[i].fieldnam);
																}
							 									else if ( io[i].fieldtyp.indexOf('grid') >=0 ){
																	io[i].sub		=(io[i]['mainview'] == io[i]['sub_view']) ? '' : 'sub' ;
																	io[i].forgnkey	= io[i]['forgnkey'] || ''; 
																	$.sys._stype[	  io[i]['mainview'] + '___'   + io[i]['mainview'] ]		= $.sys._stype[io[i]['mainview']+ '___' + io[i]['mainview']] ||  [];
																	$.sys._tablex[    io[i]['tablenam']]									=  io[i]['mainview']			+ '___' + io[i]['sub_view'];
 																	$.sys._tblaction[ io[i]['mainview'] + '___'   + io[i]['sub_view']	]	=  io[i]['fieldnam'];
																	$.sys._tblaction[ io[i]['fieldnam']	]									=  io[i]['tablenam'];
																	$.sys._gridset[   io[i]['mainview'] + '___'   + io[i]['sub_view']	]	=  { itemid :  io[i]['mainview'] + '___' + io[i]['sub_view'] , title : io[i]['el_title'],colModel :	[], searchitems	: [], sortname : io[i]['elemenid'].split('___')[1], url : "populate.php" , onSubmit : $.sys._populate ,usepager	: true  ,sortorder	:	"desc"  ,selectid  : 'selid___' + io[i]['mainview'] + '___' + io[i]['sub_view'] , itemvnum : io[i].itemvnum , vresizegrid : $.sys._vresizegrid };
																	if ( !! io[i].forgnkey) {		
																			$.sys._stype[			io[i]['mainview'] + '___'   +   io[i]['mainview']].push({ actionid  :    "d" +  io[i]['fieldnam'].substr(1),tablenam  :	 io[i]['tablenam'],pagenumb  :	"1" ,rowspage  :	"15" ,sortfild  :	 io[i]['elemenid'].split('___')[1],sortordr  :	"asc",qufield0  :	io[i].forgnkey ,quwhere0  :	"has_in",quvalue0  :	""   });
																			$.sys._tablex[  "d" +	io[i]['fieldnam'].substr(1)]  = io[i]['mainview'] + '___' +   io[i]['sub_view'];
																	}
																	grid.push(io[i]);
																}
																else 	DTM[io[i]["mainview"]][io[i]["sub_view"]]["JSONOB" ].push( io[i]) ;
														}
														for (var i =1; i <= thtm.count; i ++)						tmh += thtm[ i].list ;
																													tmh	+= '</ul>\n';
														for (var i =1; i <= thtm.count; i ++){						tmh	+=	'<div id="tabs-' + i + '" >' +	'<div id="tabs-'	 + i + '-div" style="margin: 0px; width: 100%; height:260px;"></div>' +	'<div id="subtabs-'	  + i + '" >'+	'<ul id="subtabs-list'	+ i + '" >' ;
															for (var j = 1; j <=	thtm[ i ].subtabs.count; j++)	tmh	+= 	thtm[ i ].subtabs.list[ thtm[ i ].subtabs.sequ[j] ]   || '' ;
																													tmh	+=	'</ul>';
															for (var j = 1; j <=	thtm[ i ].subtabs.count; j++)	tmh	+=  '<div id="subtabs-' + i +  j  + '" ><div id="subtabs-' + i  +  j  + '-div"></div></div>'	;
																													tmh	+=	'</div></div>' ;	
														}
														$(	tmh		).appendTo("body"	);
														$("[class^=tablist__]").click(  function(e){	$.sys._setfields(e );	} 	 );
														// appending the grid html
														for (var i =0; i < grid.length; i++)  
																	$('<p id="'  + grid[i]['mainview'] + '___' + grid[i]['sub_view'] + '" class="' + grid[i]['mainview'] + '___' + grid[i]['sub_view'] + '"> </p>').appendTo('#' + grid[i].sub + 'tabs-' + grid[i].itemvnum + '-div');

														for (var i =0; i < ga.length; i++){ // appending fields to the grid	 
																	$.sys._gridset[  ga[i]['mainview'] + '___' +   ga[i]['sub_view'] ] ['colModel'	  ].push({'name' : ga[i]['elemenid'] , 'display' : ga[i]['el_label'] ,'hidden'    : (ga[i]['modifier'] == 'hidden' ) ? true : false  ,'sortable' : true, width : ( ga[i]['sizechar'] * 4  )	, fieldord	: ga[i].fieldord, rec_guid :  ga[i].rec_guid  });
																	$.sys._gridset[  ga[i]['mainview'] + '___' +   ga[i]['sub_view'] ] ['searchitems' ].push({'name' : ga[i]['elemenid'] , 'display' : ga[i]['el_label'] ,'isdefault' : (ga[i]['modifier'] == 'defalt' ) ? true : false															, fieldord   : ga[i].fieldord, rec_guid :  ga[i].rec_guid  });
														}
														for(var each in tset){	$.sys._taxonomy[each]	=	{ name : each , items : {}	};
																for (var seach in tset[each]) 	$.sys._taxonomy[each].items[each + '___' +seach] = { name : seach};
														}
														$.sys._itemz.add.items	=  {  "tablist___tbl_clientdata" : { name : "tbl_clientdata" ,  items :		$.sys._taxonomy["tbl_clientdata"].items } };
 														$(function() {   
															for (var tab in DTM) for (var subtab in DTM[tab]) {
																	var	jht		= DTM[tab][subtab].JSONOB , sub	= (DTM[tab][subtab].itemvnum.length >  1) ? 'sub' : '' ;
																	$('<div id="view-'+ DTM[tab][subtab].itemvnum + '" class="usrsub  "></div>').appendTo( '#' + sub + 'tabs-' + DTM[tab][subtab].itemvnum + '-div'  );
																	for(var i =0; i < jht.length; i++){
							 												switch (jht[i].fieldtyp){
																			case "textarea"	 :	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><textarea rows="' + DTM[tab][subtab].JSONOB[0].size_ypx + '" cols="' + DTM[tab][subtab].JSONOB[0].size_xpx +'" title="' + DTM[tab][subtab].JSONOB[0].el_title + '" id="' + jht[0].elemenid +'" ></textarea></div>'																				   ).css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
		 							 										case "checkbox" :	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><input type ="checkbox" '											+ ' 	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span			id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >&nbsp '		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
																			case "text"		:	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><input type ="text"	             size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span			id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >&nbsp '		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
																			case "autotext"	:	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><input type ="text"	             size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   aitem"/><span		id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >&nbsp '		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
																			case "textdiv"	:	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><input type ="text"   disabled	 size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span			id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >&nbsp '		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
																			case "button"	:	$('<div id="inx-'+ jht[i].rec_guid +  '" data-rec_guid="'  + jht[i].rec_guid   + '"  ><input type ="button"			value="'		+ jht[i].el_label	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   dtmbut "/><span	id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >'								  + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue  + "px", left : jht[i].colvalue + "px"}).appendTo('#view-'+ DTM[tab][subtab].itemvnum  )	;		break;
																		};
																	}
															}
															for (var tblnam in  $.sys._dlg){	jht =  $.sys._dlg[tblnam].dlg;	
																	var  divid = 'detail___' + jht[0].mainview + "___" +  jht[0].sub_view;
																		$('<div id="' +  divid + '" class="usrsub "></div>'	 ).appendTo("#tabs" ).dialog({height:480,width:480, modal: true ,autoOpen: false});
																		for(var i =0; i < jht.length; i++){
						 													switch (jht[i].fieldtyp){
		 							 											case "checkbox" :	$('<div id="inx-'+ jht[i].rec_guid  +  '" data-rec_guid="'  + jht[i].rec_guid + '" ><input type ="checkbox" '											+ ' 	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span	id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >'		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue + "px", left : jht[i].colvalue + "px"}).appendTo('#' + divid  )	;		break;
																				case "text"		:	$('<div id="inx-'+ jht[i].rec_guid  +  '" data-rec_guid="'  + jht[i].rec_guid + '" ><input type ="text"	             size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span	id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >'		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue + "px", left : jht[i].colvalue + "px"}).appendTo('#' + divid  )	;		break;
																				case "autotext"	:	$('<div id="inx-'+ jht[i].rec_guid  +  '" data-rec_guid="'  + jht[i].rec_guid + '" ><input type ="text"	             size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   aitem"/><span	id="label-'		+ jht[i].elemenid + '" class=" itemlabel "	 >'		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue + "px", left : jht[i].colvalue + "px"}).appendTo('#' + divid  )	;		break;
																				case "textdiv"	:	$('<div id="inx-'+ jht[i].rec_guid  +  '" data-rec_guid="'  + jht[i].rec_guid + '" ><input type ="text"   disabled	 size="'	+ jht[i].sizechar	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span	id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >'		+ jht[i].el_label + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue + "px", left : jht[i].colvalue + "px"}).appendTo('#' + divid  )	;		break;
																				case "button"	:	$('<div id="inx-'+ jht[i].rec_guid  +  '" data-rec_guid="'  + jht[i].rec_guid + '" ><input type ="button"             value="'	+ jht[i].el_label	+ '"	title="'	+ jht[i].el_title + '" id="'			+ jht[i].elemenid +'"  class="   "/><span	id="label-'			+ jht[i].elemenid + '" class=" itemlabel "	 >'						  + '</span></div>').css({position: "absolute",  display : "block",  top :  jht[i].rowvalue + "px", left : jht[i].colvalue + "px"}).appendTo('#' + divid  )	;		break;
																			};						 
																		}
															}
 	  														for (var each in $.sys._gridset) {  // sorting the grids then making the dtmgrid object
																		$.sys._sortOn($.sys._gridset[each].colModel   , 'fieldord',   false  , true); 
																		$.sys._sortOn($.sys._gridset[each].searchitems, 'fieldord',   false  , true); 
																		$.sys._gridset[each].instance = $("#" +  each).dtmgrid($.sys._gridset[each])[0];
															}
															for (var __each in  $.sys._stype){ 
																		$.sys._gridset[__each].instance.grid.populate(); // populating the top grids 
																		$("#" + $.sys._gridset[__each].instance.grid._p.selectid).on({change : function (e){  $.sys._gridselect(e,$.sys._gridset[ __each	].instance.grid);}});
															}
															for(var i =1; i <= thtm.count; i++)  $("#tabs, #subtabs-" + i	).tabs();
 														}) ;
														$("#tabs").dialog({dialogClass: "no-close" , position: { my: "left top", at: "left+10 top ", of: window }, height:800,width:866, modal: true ,autoOpen: false});
														$("input").hover(  function(e){	$.sys._setActive(e,true);} , function(e){$.sys._setActive(e,false);  }).tooltip();
														$("body").delegate("input","change", function(e){	 var tbname = e.currentTarget.id.split("___")[0];
																		 if (e.currentTarget.id.indexOf("tbl_") >= 0 &&  (!!! $("#" + e.currentTarget.id  ).hasClass("aitem")) ){									
																						$.sys._dataupdate(	{ actionid : 'doupdate', tablenam : e.currentTarget.id.split("___")[0], fieldnam : e.currentTarget.id.split("___")[1], wherefld : 'rec_guid', whereval : $.sys._activrecs[tbname].rec_guid , newvalue : e.currentTarget.value   });
																		}
														       }).delegate("input","click", function(e){	var cid = e.currentTarget.id.split("___")[1] , cit = e.currentTarget.id.split("___")[0];
																						var  wherefld_		=  (!!! $.sys._dlg[cit]) ? '' : $.sys._dlg[e.currentTarget.id.split("___")[0]].forkey
																							,whereval_		=  (!!! $.sys._dlg[cit]) ? '' : $.sys._activrecs[ $.sys._dlg[e.currentTarget.id.split("___")[0]].parentbl].rec_guid;
																		if (cid == 'dodelete' ||  cid== 'comitran' || cid == 'deletran' )																					
																								$.sys._dataupdate({ actionid : cid , tablenam : e.currentTarget.id.split("___")[0], fieldnam : cid			, wherefld : 'rec_guid'	, whereval : $.sys._activrecs[e.currentTarget.id.split("___")[0]].rec_guid , newvalue : ''  });
																		if (cid == 'doinsert' )	
																								$.sys._dataupdate({ actionid : cid , tablenam : e.currentTarget.id.split("___")[0], fieldnam :  'rec_guid'	, wherefld : wherefld_  , whereval : whereval_  , newvalue : ''});
																		if (cid == 'dodelete' ||  cid== 'comitran' || cid == 'deletran' || cid == 'doinsert') $(this).prop("disabled",true);
																		if (cid == 'do_email' )	  $.sys._doemail(); 
																		if (cid == 'printout' )	  $.sys._doreports();
														});
   														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"],[id$="doinsert"]' ).attr( "disabled", true );
														$("#tabs").css('z-index' ,'99' ).dialog( "open" );
														$(".ui-button-icon-only").hide()
														$.sys._bindwheel(".aitem"); 
														// option list
														$.sys._getpst ('searchnm',{ actionid  : "searchnm",tablenam  : "tbl_savesearch",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "searchnm" ,sortordr : "asc" ,qufield0 : "searchnm" ,quwhere0 : "has_in" ,quvalue0  :	""	} ,'GET' ,function(data) {	
														 if (!!! data[0] || !!! data[0].actionid )	 return; 
														 var srchoptions = "<option value='all'>all&nbsp;&nbsp;</option>";
														 for (var i =0; i <  data.length; i++) {
															data[i].actionid  =  "plabresu";
															data[i].tablenam  = "tbl_labresultz";
															data[i].sortfild  = "date_add";
														 	srchoptions += "<option value='" +   i    + "' "   + " >"  + data[i][ "searchnm" ]  + "&nbsp;&nbsp;</option>";
														 }  
														 $.sys._optdatalist = data ;
														$("#LabResults___LabResults___searchnm").html(srchoptions);
														$("#LabResults___LabResults___searchnm").on({change : function(e){ 
															$.sys._gridset["LabResults___LabResults"].instance.grid.populate($.sys._optdatalist[ this.value]); 
														 }});});	
														$.sys._setupwheel(  'tbl_labsdetail___businame' , 'lab_name'  );
														$.sys._setupwheel(  'tbl_collectorz___businame'	, 'collname'  );
														$( '#menu___lab_name' ).on({click :  function(e ) { $.sys._searchDialog(['lab_name'] );  }});
														$( '#menu___collname' ).on({click :  function(e ) { $.sys._searchDialog(['collname'] );	 }});
														$( '#menu___clientnm' ).on({click :  function(e ) { $.sys._searchDialog(['clientnm'] );	 }});
														$( '#menu___lastname' ).on({click :  function(e ) { $.sys._searchDialog(['lastname'] );	 }});
														$( '#menu___firstnam' ).on({click :  function(e ) { $.sys._searchDialog(['firstnam'] );  }});
														$( '#menu___ssn_numb' ).on({click :  function(e ) { $.sys._searchDialog(['ssn_numb'] );  }});
														$( '#menu___nosearch' ).on({click :  function(e ) {  $.sys._clearselist();  $.sys._gridset['LabResults___LabResults' ].instance.grid.populate();   }});
														$( '#menu___otherone' ).on({click :  function(e ) {  $.sys._searchDialog(['ssn_numb','firstnam','lastname','clientnm','collname','lab_name'] );    }});
														$('body').append(	'<div title="Session Timeout" id="safeUser-dialog"  class=" loginplusdiv"  ><img  src="images/loginplus.png" class="loginplus"   /></div></div>' );
														$('#safeUser-dialog').dialog({	autoOpen: false, modal: true,closeOnEscape: false, width:246, height:253,	open: function() { $(".ui-dialog-titlebar-close").hide(); },
															buttons: {	"Cancel": function () { $(this).dialog('close');	location.reload(true) },
																		"Login": function () { $(this).dialog('close');		location.reload(true) }
					 									}});
														$( "[id^=inx-]" ).draggable().on(	{ mouseup :function( e  ) {  
																		  if ($.sys._canModify != 'user' ) //&& id.indexOf('tbl_' ) == 0)   	
																		  $.sys._getpst(	'movfld'		,{'value0' : $(this).attr('data-rec_guid') , 'value1' : this.style.top.split('px')[0],  'value2'  : this.style.left.split('px')[0]  }		,'POST' );	
																														}	});	 
														$( ".usrsub" ).droppable(		{	drop: function( event, ui ) {  }	});

														$('.fout').fadeOut(1000);
														$('.ui-widget-overlay ').css({'background': 'none'});
														$('#dvLoading').fadeOut(2000);
			}
 			,	_dataupdate			: function (item ){	setTimeout(function() {	$.sys._getpst (item.actionid,item,'GET' ,function(data) { return (!!!  data[0] || !!!  data[0].actionid ) ? false :	$.sys[ "_" + data[0].actionid ] (data[0],data[0].tablenam.substr(4)) ;});	  }, 40);
														 // this is an implicit branch table to functions:  _doupdate,_dodelete, _doinsert, _comitran,_deletran
			}				
			,	_doupdate			: function (data,ntbl){  
														$.sys._activrecs[data.tablenam].activrec[ntbl + '___' + data.fieldnam]								=	data.newvalue;
														try{	$('#' + ntbl + '___'	+  data.fieldnam + '___' +  data.whereval )[0].firstChild.innerHTML	=	data.newvalue ;}catch(e){}
														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"],[id$="doinsert"]' ).attr( "disabled", false );
														$.sys._db[data.whereval].data.rows[$.sys._db[data.whereval].index][ntbl + '___' +	data.fieldnam]	=	data.newvalue; 
			}			
			,	_dodelete			: function (data,ntbl){	
														for (var each in  $.sys._activrecs[data.tablenam].activrec){
															try{    $('#' + ntbl			+ '___' +  each.split('___')[1] + '___' +   data.whereval )[0].firstChild.innerHTML = '' ;
																	$('#' + data.tablenam	+ '___' +  each.split('___')[1]).val(  '') ;	  }catch(e){}
																	$.sys._activrecs[data.tablenam].activrec[each]= '';
														}
																	$.sys._activrecs[data.tablenam].rec_guid = '';
																	$.sys._db[data.whereval].data.rows.splice(  $.sys._db[data.whereval].index,1 )  ;
														var rows =  $.sys._db[data.whereval].data.rows
														   ,page =  $.sys._db[data.whereval].data.page
														   ,totl =  $.sys._db[data.whereval].data.total;
																	$.sys._db[data.whereval].data = {};
  														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"]' ).attr( "disabled", true );   $('[id$="doinsert"]' ).attr( "disabled", false ); 		 
														$.sys._gridset[	$.sys._tablex[data.tablenam ]].instance.grid.addData({"page":page,"total":  totl ,"rows":  rows},true	);	
			}			
			,	_doinsert			: function (data,ntbl){ 	 var dialid = (!!! $.sys._dlg[ data.tablenam]) ? "#tabs" : '#detail___' + $.sys._activrecs[data.tablenam].mainview + "___" + $.sys._activrecs[data.tablenam].sub_view ;
														for (var each in  $.sys._activrecs[data.tablenam].activrec){  
																$.sys._activrecs[data.tablenam].activrec[each] = ''
																try{	
																	$('#' + data.tablenam + '___' +  each.split('___')[1],dialid  ).val( '' ) ;  }catch(e){}
														}
														$.sys._activrecs[data.tablenam].rec_guid									=  data.rec_guid;
														$.sys._activrecs[data.tablenam].activrec[ntbl + '___rec_guid']				=  data.rec_guid;
														$('#' + data.tablenam + '___rec_guid' ,dialid   ).val( data.rec_guid ) ;
														var recg = $("td:first", "#" + $.sys._tablex[	data.tablenam ] )[0].id.split("___")[2];
														$.sys._dbhashadd(recg,data.rec_guid);
  														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"]' ).attr( "disabled", false );  $('[id$="doinsert"]' ).attr( "disabled", true ); 														
														if(!! data.wherefld){
															$.sys._activrecs[data.tablenam].activrec[ntbl + '___' + data.wherefld ]	= data.whereval;
															$('#' + data.tablenam + '___' + data.wherefld ,dialid  ).val( data.whereval ) ;
															$.sys._db[ data.rec_guid].data.rows[$.sys._db[ data.rec_guid].index][ntbl + '___' +	 data.wherefld]	=	data.whereval;
														}
			}
			,_comitran			: function (data ){		$.sys._deletran(data );
			}	
			,_deletran			: function (data){ var dialid = (!!! $.sys._dlg[ data.tablenam]) ? '#tabs' : '#detail___' + $.sys._activrecs[data.tablenam].mainview + "___" + $.sys._activrecs[data.tablenam].sub_view;
															for (var each in  $.sys._activrecs[data.tablenam].activrec){  
																			  $.sys._activrecs[data.tablenam].activrec[each] = ''
																	try{	 $('#' + data.tablenam + '___' +  each.split('___')[1], dialid ).val( '' ) ; }catch(e){}
																}	
  														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"]' ).attr( "disabled", true );   $('[id$="doinsert"]' ).attr( "disabled", false ); 
			}			
			,_vresizegrid		: function (num,newH){  if (num < 10) $("#tabs-" + num + "-div").css({	height: newH	});
			}
			,_resettran			: { actionid : "deletran", tablenam : "tbl_clientdata", fieldnam : "deletran", wherefld : 'rec_guid', whereval :  '' , newvalue : "Reset" 
			}		
			,_setfields			: function (e ){	 		var avalue = e.currentTarget.id.split('___')
															, fvalue = avalue[0] +  '___' + avalue[3]
															, tabval = avalue[1] + '___'  + avalue[3]
															, tablist = 	{	 LabResults___Labs 	: 'tbl_labsdetail___businame',   LabResults___Collectors: 'tbl_collectorz___businame'  };
															$.sys._selist.active		=   (!! tablist[tabval])  ?  tablist[tabval] : '';
															$('[id$="comitran"],[id$="deletran"],[id$="dodelete"],[id$="doinsert"]' ).attr( "disabled", true ); 
											 				$.sys._dataupdate($.sys._resettran);
															$.sys._resettran.tablenam	=	avalue[1];
 															$.sys._itemz.add.items = {  fvalue	: { name : 	$.sys._resettran.tablenam	,  items :		$.sys._taxonomy[$.sys._resettran.tablenam].items } };
 			}
			,_clearselist		: function(act,stat)	{	for (var each in $.sys._selist) 
															$.sys._selist[each].value	=  ( each != 'active' && each != 'sestatus')  ? '' :  $.sys._selist[each].value  ;
															$.sys._selist.active		= act  || '';		
															$.sys._selist.sestatus		= stat || {};
															$.sys._gridset["LabResults___LabResults"].instance.grid._p.xtra ='';	
			}
			,_setupwheel		: function(i1,i2 ) {		if ($.sys._selist[i1].onex == 'yes') 	return ; 
															var  itm = i1, itm2 = i2 || '';		
																			$.sys._selist[itm ].onex		= ( $.sys._selist[itm ].onex == 'no') ? 'yes' :  (( 1 *  $.sys._selist[itm ].onex ) + 1 );
															if (!! itm2)	$.sys._selist[itm2].onex		= ( $.sys._selist[itm2].onex == 'no') ? 'yes' :  (( 1 *  $.sys._selist[itm2].onex ) + 1 );
															$.sys._getpst ( $.sys._selist[itm ].itms.actionid,	$.sys._selist[itm ].itms,'GET' ,function(data) {	if (!!! data[0] || !!! data[0].actionid )	 return; 
															$.sys._selist[itm].list =[]; 
															for (var i =0; i <  data.length; i++) {	 
																	 if (!! data[i][$.sys._selist[itm].qname])
																					$.sys._selist[itm].list.push( data[i][$.sys._selist[itm].qname]);
															}
															 $.sys._selist[itm].data  = data;
															if (itm2){	$.sys._selist[itm2].list = $.sys._selist[itm].list ;
																		$.sys._selist[itm2].data = data;
															}
															});
			}
			,_bindwheel			: function(cls) {		
														$( cls ).bind( "keydown", function( e ) {  if ( e.keyCode === $.ui.keyCode.TAB &&  $( this ).data( "ui-autocomplete" ).menu.active )    e.preventDefault();})
															.autocomplete({minLength: 0	 ,focus : function(e){ return false; } 
															,select: function(e,ui){    $.sys._selist[$.sys._selist.active].value = ui.item.value;
																						this.value =ui.item.value;  	return false;	
															}
															,source: function( req, out){		if (  !! $.sys._selist.active ) 
																								if (  !! $.sys._selist[ $.sys._selist.active])
																								if (  !! $.sys._selist[ $.sys._selist.active].list.length )		
																		out( $.ui.autocomplete.filter(	 $.sys._selist[ $.sys._selist.active].list ,req.term.split( /,\s*/ ).pop()));
															  }})
															.on({ click : function (e){ 	
																						$.sys._selist.active = 	e.currentTarget.id;  
																						$.sys._setupwheel( $.sys._selist.active  ); }
																,change : function(e){  $.sys._selist.active = 	e.currentTarget.id
																						$.sys._selist[$.sys._selist.active].value = $('#' +	e.currentTarget.id).val();
																}});
			}
			,_dosearchxtra		: function( aid  ){		var xlist = [], xtra = {};
														for (var each in $.sys._selist){ 	if (each == 'active' || each == 'sestatus'  ||	each.indexOf("tbl_" ) >=0)	continue;
																							if ( !! $.sys._selist[each].value ) xlist.push(each);
														}
														if (xlist.length > 0) {		xtra.quwhere0  = 'has_in'; 
																					xtra.qufield0  =  $.sys._selist[xlist[0] ].qname; 	xtra.quvalue0  =  $.sys._selist[xlist[0] ].value;
														}
														if (xlist.length > 1) {		xtra.quwhere0  = 'has_in';
																					xtra.qufield0  =  $.sys._selist[xlist[1] ].qname;   xtra.quvalue0  =  $.sys._selist[xlist[1] ].value;
														}
														if (xlist.length > 2) {		xtra.quwhere0  =  'has_in';
																					xtra.qufield0  =  $.sys._selist[xlist[2] ].qname;   xtra.quvalue0  =  $.sys._selist[xlist[2] ].value;
														}
														if (xlist.length > 3) {		xtra.quwhere0  =  'has_in';
																					xtra.qufield0  =  $.sys._selist[xlist[3] ].qname;  	xtra.quvalue0  =  $.sys._selist[xlist[3] ].value;  
														}
														if (xlist.length > 4) {		xtra.quwhere0  =  'has_in';
																					xtra.qufield0  =  $.sys._selist[ xlist[4]].qname;   xtra.quvalue0  =  $.sys._selist[xlist[4] ].value;
														} 
														if (!! aid) { 	for (var each in aid) xtra[each] = aid[each];
														}
														 return xtra;
			}
			,_selist  :								{   active  : ''  , sestatus : {}
													  , lab_name						: {qname : 'labsdetail___businame' ,list : [],rec : [], value : '' , onex : 'no'	,itms : { actionid  : "plablabs",tablenam  : "tbl_labsdetail",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , collname						: {qname : 'collectorz___businame' ,list : [],rec : [], value : '' , onex : 'no' 	,itms : { actionid  : "pcolcols",tablenam  : "tbl_collectorz",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , clientnm						: {qname : 'clientdata___businame' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "pclients",tablenam  : "tbl_clientdata",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , lastname						: {qname : 'clientempl___lastname' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "dcliempl",tablenam  : "tbl_clientempl",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "lastname" ,sortordr : "asc" ,qufield0 : "lastname" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , firstnam						: {qname : 'clientempl___firstnam' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "dcliempl",tablenam  : "tbl_clientempl",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "firstnam" ,sortordr : "asc" ,qufield0 : "firstnam" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , ssn_numb						: {qname : 'clientempl___ssn_numb' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "dcliempl",tablenam  : "tbl_clientempl",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "ssn_numb" ,sortordr : "asc" ,qufield0 : "ssn_numb" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , searchnm						: {qname : 'savesearch___searchnm' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "searchnm",tablenam  : "tbl_savesearch",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "searchnm" ,sortordr : "asc" ,qufield0 : "searchnm" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , searchin						: {qname : 'savesearch___searchnm' ,list : [],rec : [], value : '' , onex :  0 		,itms : { actionid  : "searchin",tablenam  : "tbl_savesearch",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "searchnm" ,sortordr : "asc" ,qufield0 : "searchnm" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , tbl_labsdetail___businame		: {qname : 'labsdetail___businame' ,list : [],rec : [], value : '' , onex : 'no'	,itms : { actionid  : "plablabs",tablenam  : "tbl_labsdetail",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , tbl_collectorz___businame		: {qname : 'collectorz___businame' ,list : [],rec : [], value : '' , onex : 'no'	,itms : { actionid  : "pcolcols",tablenam  : "tbl_collectorz",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
													  , tbl_clientdata___businame		: {qname : 'clientdata___businame' ,list : [],rec : [], value : '' , onex : 'no'	,itms : { actionid  : "pclients",tablenam  : "tbl_clientempl",pagenumb  :  "1" ,rowspage  :  "1000" ,sortfild  :  "businame" ,sortordr : "asc" ,qufield0 : "businame" ,quwhere0 : "has_in" ,quvalue0  :	""	}}
			}
			,_searchDialog		: function(item ){		var  id='LabResults',  divid = '#search___'  + id   
														,h =  '<div id="search___' + id + '"  class="dialogsearch search___' + id +'" >'
														  +	 '<div><input type="button" id="searchsearch"   value="Search" 	class="searchsearch" />' 
														  +	 '<input type="button"		id="searchclear"	value="Clear" 	class="searchclear"  />' 
														  +	 '<input	size="10" 		id="searchname"		value=""		class="dsavesearch"  />'
														  +  '<input type="button"		id="savesearch"	    value="Save" 	class="savesearch"   /></div>';
														for (var i =0; i < item.length; i++){
															if (item[i] == 'lab_name') h  +=  '<div><input id="lab_name" value="" size="30" class="dsearch" placeholder="Lab Name" />Lab Name   </div>'
															if (item[i] == 'collname') h  +=  '<div><input id="collname" value="" size="30" class="dsearch" placeholder="Collector Name" />Collector  </div>'
															if (item[i] == 'clientnm') h  +=  '<div><input id="clientnm" value="" size="30" class="dsearch" placeholder="Client Name" />Client Name</div>'
															if (item[i] == 'lastname') h  +=  '<div><input id="lastname" value="" size="30" class="dsearch" placeholder="Last Name" />Last Name  </div>'
															if (item[i] == 'firstnam') h  +=  '<div><input id="firstnam" value="" size="30" class="dsearch" placeholder="First Name" />First Name </div>'
															if (item[i] == 'ssn_numb') h  +=  '<div><input id="ssn_numb" value="" size="30" class="dsearch" placeholder="SS Num" />Social SN  </div>'
														}			 
														 h  +=   '</div>';
														 var heig =  item.length * 30 + 40;
														 if (!! $(divid ).length ){ $(divid ).html(h); 
															if (item.length == 1 && item[0] != $.sys._selist.active) 
																$.sys._clearselist();
															else{	for (var i =0; i < item.length; i++){
																		 $.sys._selist.sestatus[ item[i]] = $.sys._selist[item[i]].value;
																		$('#' + item[i] ).val( $.sys._selist[item[i]].value);
																	}
																}
														 }
														 else $( h  ).appendTo("#tabs" ).dialog({height: heig ,width:300, modal: false ,autoOpen: false});
														$.sys._selist.active = item[0];
														$.sys._bindwheel(".dsearch");														
														$('#searchsearch' ).on({ click : function(divid) { 
																					$.sys._gridset["LabResults___LabResults"].instance.grid.populate( $.sys._dosearchxtra());	
																			}});	
														$('#searchclear'  ).on({ click: function(divid ) {		 $(".dsearch" ).val('');    $.sys._clearselist();		}		});
														$('#savesearch'   ).on({ click: function(e ) {	 if (!!!  $("#searchname" ).val() ) return;	
																					var itms = 	$.sys._dosearchxtra( {actionid : "searchin",searchnm : $("#searchname" ).val() });
																					$.sys._getpst ("searchin",itms,'GET' ,function(data) {	 if (!!! data[0] || !!! data[0].actionid )	 return; });
														 } });
														$(divid ).css({'background-color' : 'lightgrey'} ).dialog( "open" );
			}
			,_doDetail			: function(t) {			var table0='',cells	= 	t.context.cells ,itms	= $.sys._stype[ t.context.offsetParent.firstChild.className]  || [], dialid = (!!  itms.length ) ? "#tabs" :'#detail___' + t.context.offsetParent.firstChild.className  ;	 
														for(var i =0 ; i < cells.length; i++){	var	elemen_id	= cells[i].abbr,field = cells[i].abbr.split("___")[1],value0 = cells[i].firstChild.innerHTML;
															    table0		= 'tbl_'+ cells[i].abbr.split("___")[0]	;
																$.sys._activrecs[table0].activrec[elemen_id]	=  value0;
																if	(  'rec_guid'  == field )	$.sys._activrecs[table0].rec_guid				=  value0;
																if	(  'rec_guid'  == field && !! itms && !! itms.length) {
																		for (var j=0; j< itms.length; j++ )  itms[j].quvalue0	=  value0;
																}
														}
														var fullrec =  $.sys._db[$.sys._activrecs[table0].rec_guid].data.rows[$.sys._db[$.sys._activrecs[table0].rec_guid].index];
														if (!!!  itms.length )  
															$(dialid ).dialog( "open" );
  														$('[id$="comitran"],[id$="deletran"],[id$="dodelete"],[id$="doinsert"]' ).attr( "disabled", false ); 
														for (var each in   fullrec){		if (each.indexOf('___') > 0)
																try{  $(	'#tbl_'+ each ,dialid  ).val( fullrec[each]);  	 } catch (e) { 	}
														}
														 for (var j=0; j< itms.length; j++ ) {
															$.sys._getpst (itms[j].actionid,itms[j],'GET' ,function(data) {	 if (!!! data[0] || !!! data[0].actionid )	 return;
																		$.sys._gridset[		$.sys._tablex[	data[0].actionid ]		].instance.grid.addData({"page":"1","total": data.length ,"rows": data},true	);	 
																		});
													}
		}
		,_populate : function (g,xtra) {				g._p.newp =  (!! g._p.newp) ? g._p.newp :  1;	if (g._p.page > g._p.pages)   	g._p.page = g._p.pages; 
													var  sortfild	=  (!! g._p.sortname				)  ?  g._p.sortname				:  g._p.qtype.split('___')[1] 
														,sortordr	=  (!! g._p.sortorder				)  ?  g._p.sortorder			:  'asc'
														,qufield0	=  (!! g._p.qtype.split('___')[1]	)  ? g._p.qtype.split('___')[1]	: ''	
														,quvalue0	=  (!!	g._p.query					)  ? g._p.query					: ''
														 ,	gp		= {	 actionid : $.sys._tblaction[g._p.itemid]  
																		,tablenam : $.sys._tblaction[ $.sys._tblaction[g._p.itemid]]	
																		,pagenumb :	g._p.newp.toString()
																		,rowspage :	g._p.rp.toString()
																		,sortfild :	sortfild
																		,sortordr :	sortordr
																		,qufield0 : qufield0
																		,quwhere0 : 'has_in'
																		,quvalue0 :	quvalue0	 
														} ;
														g._p.xtra = xtra || g._p.xtra;
														if (!! g._p.xtra ){	for(var each in g._p.xtra )	
																gp[each] = g._p.xtra [each];
														}
														$.sys._getpst(gp.actionid, gp,'POST',   function (data){ 	 
																$.sys._gridset[g._p.itemid ].instance.grid.addData({"page": parseInt (data[0].recnumbr / $.sys._gridset[g._p.itemid ].instance.grid._p.rp) + 1 ,"total": data[0].reccount ,"rows": data}	);	 
														});
		}
		,_setActive			: function (e,o){			if (o){ for (var each in $.sys._hotEl)  $.sys._hotEl[each] = e.currentTarget.id;
																$.sys._hotEl[ e.currentTarget.id] = e.currentTarget.id;
														}else	$.sys._hotEl[ e.currentTarget.id] = 'last out';
		}
		,_RegisterUser		: function ()	{			if (!!! $( "#regpage" ).length ) {
														$(function() {
//swb
															$('<div	 id="regpage"  class="regpage" > </div>').appendTo("body").dialog({ height: 750, width: 910, modal: true, autoOpen: false });
															$(  '<div id="firstn-div" class="firstn-div"	><input name="First_Name"		type="text"		  id="firstn-input"		class="firstn-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="lastnm-div" class="lastnm-div"	><input name="Last_Name"		type="text"		  id="lastnm-input"		class="lastnm-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="usernm-div" class="usernm-div"	><input name="username"			type="text"		  id="usernm-input"		class="usernm-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="email_-div" class="email_-div"	><input name="Email"			type="text"		  id="email_-input"		class="email_-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+	 '<div id="gender-div" class="gender-div"	><select name="Gender" 							  id="gender-input"		class="gender-input" style="font-size: 18px; border:0;"  		required			 > <option selected="selected" value="">Select</option><option value="Male">Male</option><option value="Female">Female</option></select></div> '
															+    '<div id="pas_wd-div" class="pas_wd-div"	><input name="Password"			type="password"   id="pas_wd-input"		class="pas_wd-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="passbk-div" class="passbk-div"	><input name="confirmPassword"	type="password"   id="passbk-input"		class="passbk-input" style="font-size: 18px; border:0;"			required  			/></div>'
															+    '<div id="birthd-div" class="birthd-div"	><input name="DateOfBirth"		type="date"		  id="birthd-input"		class="birthd-input" style="font-size: 18px; border:0;"			required			/></div>'
															+    '<div id="phone1-div" class="phone1-div"	><input name="Phone1"			type="text"		  id="phone1-input"		class="phone1-input" style="font-size: 18px; border:0;"			         size="13"	/></div>'
															+    '<div id="addres-div" class="addres-div"	><input name="Address"			type="text"		  id="addres-input"		class="addres-input" style="font-size: 18px; border:0;"			         size="13"	/></div>'
															+    '<div id="pquest-div" class="pquest-div"	><input name="PasswordQuestion"	type="text"		  id="pquest-input"		class="pquest-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="panswr-div" class="panswr-div"	><input name="PasswordAnswer"	type="text"		  id="panswr-input"		class="panswr-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+    '<div id="panswr-div" class="admine-div"	><input name="AdminEmail"	type="text"		  id="admine-input"		class="admine-input" style="font-size: 18px; border:0;"			required size="13"	/></div>'
															+ '<div id="Submit-div" class="Submit-div"  ></div><div id="Reset-div"	 class="Reset-div"></div>'
															 ).appendTo("#regpage" ); // 	<input type="checkbox" name="IAgree" id="IAgree" /> // admin email (submit will send an email with a sign up guid to the admin *>
														$('#Submit-div').on({ click : function() { 
																			$.sys._getpst( "regist", {	
																						  'firstn'  :	$("#firstn-input").val()
																						, 'lastnm'  :   $("#lastnm-input").val()
																						, 'usernm'	:	$("#usernm-input").val() 
																						, 'email_'	:	$("#email_-input").val()
																						, 'gender'	:	$("#gender-input").val() 
																						, 'pas_wd'	:	$("#pas_wd-input").val()
																						, 'passbk'	:	$("#passbk-input").val() 
																						, 'birthd'	:	$("#birthd-input").val()
																						, 'phone1'	:	$("#phone1-input").val()
																						, 'addres'	:	$("#addres-input").val()
																						, 'pquest'  :   $("#pquest-input").val()
																						, 'admine'  :   $("#admine-input").val()  
																						, 'panswr'	:	$("#panswr-input").val() },"GET");
																						}
																						}) ;
//swb							 
														$('#Reset-div' ).on({ click: function() {	alert("ready to reset");	}	 }); 
														}) ;
														}
														$( "#regpage"	).css({'z-index' : 121, 'width' : '820px'} ).dialog( "open" );
		},_doemail	: function(){
														$('<div	 id="sendemail"  class="sendemail" > '
													 		+	'<div id="emailfro-div" class="mailfrom-div"	><input id="emailfro-but" class="mailfrom-but" type="button" value="From:" disabled />					<input name="From:"			type="text"		disabled		id="mailfrom-text"		class="mailfrom-text"   size="25" value="user@userdtm.com"	/></div>'
															+	'<div id="email_to-div" class="email_to-div"	><input id="email_to-but" class="email_to-but" type="button" value="&nbsp;&nbsp;To:&nbsp;" />			<input name="To: "			type="text"						id="email_to-text"		class="email_to-text"   size="73"	/></div>'
															+	'<div id="emailsub-div" class="mailsubj-div"	><input id="mailsubj-but" class="mailsubj-but" type="button" value="Subj:" disabled />					<input name="Subj:"			type="text"						id="mailsubj-text"		class="mailsubj-text"   size="73"	/></div>'
															+	'<div id="emailbod-div" class="emailbod-div"	>	<textarea  rows="27" cols="81"		 id="mailtext-input"		class="mailtext-input"    ></textarea></div>'
 		 													+	 '</div>' ).appendTo("#tabs" ).dialog({height:580,width:550, modal: true ,autoOpen: true,buttons	: {	"Cancel": function() {	$(this).dialog('close');  	},"Send": function () {
				 															$.sys._getpst('sendmail', {
				 																		  'actionid': 'sendmail'
																						, 'fromname': 'username'					 
																						, 'emailfro': $("#mailfrom-text").val()
																						, 'email_to': $("#email_to-text").val()
																						, 'emailsub': $("#mailsubj-text").val()
																						, 'emailbod': $("#mailtext-input").val()
																						, 'datesent': ''
																						, 'oktobatc': '1'}, "POST");
                                                                                  $(this).dialog('close');    }}
															});
		},_safeUser			: function(   )	{			clearTimeout($.sys._diaTime ); $.sys._diaTime =	setTimeout(function(){	$('#safeUser-dialog').dialog('open'); }, $.sys._expired);
		}, _login			: function(){				setTimeout(function() { $('#dvLoading').show(); }, 50);
														$.sys._username =		$('.userid-input').val();
														setTimeout(function() {	$.sys._getpst("loginu",{username :  $.sys._username ,password : $(".passwd-input").val() },'GET',function (data) {	
																											if ( !! $("#tabs").length) $( "#tabs" ).css('z-index',99).dialog( "open" );
																											else 	$.sys._setDTMApp( (typeof (data) == "string") ? eval ('('+  data + ')' ) : data);
																											$('#dvLoading').fadeOut(2000); 
																									}); } 	, 100);
		}
		, _setran: function (a, l, gfd) {
														var 	userguidval = (!!  window.sessionStorage.getItem('userguid') &&  window.sessionStorage.getItem('userguid')  !== "undefined" ) ?   window.sessionStorage.getItem('userguid')  :   "00000000-0000-0000-0000-000000000000",
														 parms ={ actionid : a, ret_code : "ok"
														, rec_guid : window.sessionStorage.getItem('rec_guid')  ||  this._cookash['rec_guid']		|| 	"00000000-0000-0000-0000-000000000000"
														, mro_guid : window.sessionStorage.getItem('mro_guid')  ||  this._cookash['mro_guid']	 	 		 
														, ses_guid : window.sessionStorage.getItem('ses_guid')  ||  this._cookash['ses_guid']			  
														, tranguid : window.sessionStorage.getItem('tranguid')  ||  this._cookash['tranguid']
														, userguid : userguidval
														, time_out : $.sys._datime.toUTCString() 
														, fieldefs : gfd	|| ''   
														};
																	return (!! l)  ?  $.extend( parms, l ) : parms;
		}, _setses			: function(p)		{  if (!!! p ) return;
														$.sys.set('rec_guid', (!! p.rec_guid) ? p.rec_guid : '' );	$.sys.set('mro_guid', (!! p.mro_guid) ? p.mro_guid : '' );
														$.sys.set('ses_guid', (!! p.ses_guid) ? p.ses_guid : '' );	$.sys.set('tranguid', (!! p.tranguid) ? p.tranguid : '' );
														$.sys.set('ret_code', (!! p.ret_code) ? p.ret_code : '' );  $.sys.set('userguid', (!! p.ret_code) ? p.userguid : '' );
		}
		, _getpst			: function(act,itms,gp,cb,gfd){ //   timeout:8000, 
												 		$.ajax({type: gp ,url: 'dtmall.php',data:{"str" : JSON.stringify(	$.sys._setran( act,itms,gfd ) )} ,dataType: 'json',error: function (XMLHttpRequest, textStatus, errorThrown) {	try {	alert( gp  + " " + act   + "\ntextStatus: " +textStatus + "\nerrorThrown: " + errorThrown);	} catch (e) {}}
	 													,success:	function (data) {		if (typeof (data) == "string") data = eval ('('+  data + ')' );
																							 if (data[0].ret_code !== 'ok')  alert("action: " + data[0].actionid + "\nret_code is missing or bad : " + data[0].ret_code); 
																							 $.sys._setses(data[0]);
																							 $.sys._safeUser();
																							 return (!! cb && !! data[0] ) ? cb(data) :  data;
	 														}});
			}
	};
});
 
