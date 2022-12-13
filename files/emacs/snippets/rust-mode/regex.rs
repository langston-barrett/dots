let re = Regex::new(r"(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})").unwrap();
let before = "2012-03-14, 2013-01-01 and 2014-07-05";
let after = re.replace_all(before, "$m/$d/$y");
assert_eq!(after, "03/14/2012, 01/01/2013 and 07/05/2014");