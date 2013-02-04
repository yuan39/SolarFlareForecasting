import java.sql.*;
import java.util.Properties;
import java.io.*;
import java.util.Scanner;

public class DBDemo
{
  // The JDBC Connector Class.
  private static final String dbClassName = "com.mysql.jdbc.Driver";

  // Connection string. emotherearth is the database the program
  // is connecting to. You can include user and password after this
  // by adding (say) ?user=paulr&password=paulr. Not recommended!

  private static final String CONNECTION =
                          "jdbc:mysql://127.0.0.1/daily";

  public static void main(String[] args) throws
                             ClassNotFoundException,SQLException
  {
	System.out.println(dbClassName);
		// Class.forName(xxx) loads the jdbc classes and
   	 // creates a drivermanager class factory
	Class.forName(dbClassName);

    // Properties for user and password. Here the user and password are both 'paulr'
	Properties p = new Properties();
	p.put("user","root");
	p.put("password","vsowebserver");

    // Now try to connect

	Connection con = null;
	Scanner scan=null;
        try{
        con = DriverManager.getConnection(CONNECTION,p);
	//Statement stmt = con.createStatement();

	
	
		scan = new Scanner(new File("/var/www/daily.txt"));
	
		while(scan.hasNextLine()){
			String aline = scan.nextLine();
			String str[]= aline.split(",");
			if (str.length == 12){
				for(int i = 0; i< str.length; i++){
					//System.out.println(str[i]);
				
				}


String query = "insert into flares(fdate, ar,helicity,flux,index24,index48,c24,c48,m24,m48,x24,x48) values(?,?,?,?,?,?,?,?,?,?,?,?)";

      PreparedStatement pstmt = con.prepareStatement(query);
	System.out.println(str[0]);
      java.sql.Date mydate = java.sql.Date.valueOf(str[0]);

      pstmt.setDate(1, mydate);
      
      pstmt.setInt(2, Integer.parseInt(str[1]));pstmt.setDouble(3, Double.parseDouble(str[2]));pstmt.setDouble(4, Double.parseDouble(str[3]));pstmt.setDouble(5, Double.parseDouble(str[4]));pstmt.setDouble(6, Double.parseDouble(str[5]));pstmt.setDouble(7, Double.parseDouble(str[6]));pstmt.setDouble(8, Double.parseDouble(str[7]));pstmt.setDouble(9, Double.parseDouble(str[8]));pstmt.setDouble(10, Double.parseDouble(str[9]));pstmt.setDouble(11, Double.parseDouble(str[10]));pstmt.setDouble(12, Double.parseDouble(str[11]));

      // execute query, and return number of rows created
      int rowCount = pstmt.executeUpdate();
      System.out.println("rowCount=" + rowCount);
				//stmt.executeUpdate("insert into flares values('"+str[0]+"',"+str[1]+","+
                             	//str[2]+","+str[3]+","+str[4]+","+str[5]+","+str[6]+","+str[7]+","+str[8]+","+str[9]+","+
				//str[10]+","+str[11]+", )");

				
			}
		}
	}catch(Exception e){
		System.out.println(e.toString());
	}
	finally{
		scan.close();
                con.close();
	}



   
   
              
    	//System.out.println("It works !");
    	}
    
}
