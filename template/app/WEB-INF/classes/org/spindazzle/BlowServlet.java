// --------------------------------------------------------------------------
// BlowServlet.java - The blow servlet wrapper
// Copyright (C) 2007, 2009  Anthony Green <green@spindazzle.org>
// 
// This file is part of BLOW - A Web Application Framework in LISP.
//
// BLOW is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 3, or
// (at your option) any later version.
//
// BLOW is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with BLOW; see the file COPYING.LESSER.  If not,
// write to the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.
//
// This first version of this file was copied from ABCL-web, which is
// distributed under ther terms of the LGPL.
// See http://abcl-web.sourceforge.net for details.
// --------------------------------------------------------------------------

package org.spindazzle;

import org.armedbear.lisp.*;

import java.io.*;
import java.util.*;

import javax.servlet.http.*;
import javax.servlet.*;

public class BlowServlet extends HttpServlet 
{
  private static Interpreter interpreter;
  private static Function lispServiceFun = null;
  Function lispServiceGetFun = null;
  LispObject debuggerHook = null;
  
  public static void serveStaticContent(String filename, OutputStream os) throws IOException
  {
    FileInputStream fis  = new FileInputStream(new File(filename));
    byte[] buf = new byte[1024];
    int i = 0;
    while((i=fis.read(buf))!=-1) 
      os.write(buf, 0, i);
    fis.close();
  }
  
  public void init(ServletConfig config) throws ServletException
  {
    super.init(config);
    interpreter = Interpreter.getInstance();
    if (interpreter == null) {
      log("making new interpreter");
      interpreter = Interpreter.createInstance();
    }
    ;
    LispThread thread = LispThread.currentThread();
    Symbol dbgrhkfunSym = Lisp.PACKAGE_SYS.findAccessibleSymbol("%DEBUGGER-HOOK-FUNCTION");
    debuggerHook = dbgrhkfunSym.getSymbolFunction();
    // thread.bindSpecial(Symbol.DEBUGGER_HOOK, debuggerHook);
	
    Load.load(config.getServletContext().getRealPath("WEB-INF/lisp/libs/_blowboot.lisp"));
    
    org.armedbear.lisp.Package blowPackage = Packages.findPackage("BLOW");
    lispServiceGetFun = (Function)blowPackage.findExternalSymbol(new SimpleString("SERVICE-HTTP-GET-REQUEST")).getSymbolFunction();
  }

  public void doGet (HttpServletRequest req,
		     HttpServletResponse res)
      throws ServletException, IOException
    {
	HttpSession session = req.getSession();
	
	LispThread thread = LispThread.currentThread();
	// thread.bindSpecial(Symbol.DEBUGGER_HOOK, debuggerHook);
	
	LispObject usession = (LispObject) session.getAttribute("blow");
	if (usession == null)
	    {
		org.armedbear.lisp.Package webappPackage = Packages.findPackage("WEBAPP");
		lispServiceFun = (Function)webappPackage.findExternalSymbol(new SimpleString("MAKE-USER-SESSION")).getSymbolFunction();
		usession = lispServiceFun.execute();
	    }
	// log ("MAKE-USER-SESSION => " + usession.toString());
	  
	session.setAttribute("blow", usession);
	
	lispServiceGetFun.execute(new JavaObject(req), new JavaObject(res), usession);
    }
}
