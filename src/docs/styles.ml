let styles = {|
* {
  box-sizing: border-box;
}

body {
  background: #fff;
  color: #222;
  font-family: Circular Std, sans-serif;
  line-height: 1.15;
  -webkit-font-smoothing: antialiased;
  margin: 0;
  font-size: 1.0625rem;
}

.keyword {
  color: #264059;
}

.type {
  color: #ad448e;
}

.parameter {
  color: #264059;
}

.classname {
  color: #2c8093;
}

.fnname {
  color: #9a6e31;
}

.sidebar {
  width: 200px;
  position: fixed;
  left: 0;
  top: 0;
  bottom: 0;
  overflow: auto;

  background-color: #F1F1F1;
}

.documentation {
  margin-left: 230px;
}

.sidebar > ul {
  margin: 0 10px;
  padding: 0;
  list-style: none;
}

.sidebar a {
  display: block;
  text-overflow: ellipsis;
  overflow: hidden;
  line-height: 15px;
  padding: 7px 5px;
  font-size: 14px;
  font-weight: 400;
  transition: border 500ms ease-out;
  color: #000;
  text-decoration: none;
}

.sidebar h3 {
  border-bottom: 1px #dddddd solid;
  font-weight: 500;
  margin: 20px 0 15px 0;
  padding-bottom: 6px;
  text-align: center;
}

.declaration {
  border-bottom: 1px solid #f0f0f0;
}

.declaration :last-child {
  border: none;
}

h4.function-declaration {
  font-weight: 600;
  margin-top: 16px;
}

h4.value-declaration {
  font-weight: 600;
  margin-top: 16px;
}

h4.type-declaration {
  font-weight: 600;
  margin-top: 16px;
}

h4.class-declaration {
  font-weight: 600;
  margin-top: 16px;
}
|}
