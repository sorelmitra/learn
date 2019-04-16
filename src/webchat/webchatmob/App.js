import React from 'react';
import { StyleSheet, Text, View, 
  Image, FlatList, TextInput, Keyboard,
  Dimensions } from 'react-native';

const heights = {
  logo: 150,
  head: 15,
  subtext: 10,
  input: 15,
  conv: 30,
  padding: 10
};

export default class App extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      input: "",
      data: [],
      index: 1,
      messageTypes: [],
      listStyle: {height: this.regularListHeight()}
    }
  }

  componentDidMount() {
    var f = this.keyboardDidShow.bind(this);
    this.keyboardDidShowListener = Keyboard.addListener(
      'keyboardDidShow',
      f,
    );
    f = this.keyboardDidHide.bind(this);
    this.keyboardDidHideListener = Keyboard.addListener(
      'keyboardDidHide',
      f,
    );
    //this._textInput.focus();
  }

  componentDidUpdate() {
  }

  heightsSum() {
    var sum = 0;
    for (var n in heights) {
      sum += heights[n];
    }
    return sum;
  }

  regularListHeight() {
    windowHeight = Dimensions.get('window').height;
    regularHeight = windowHeight - this.heightsSum();
    return regularHeight;
  }

  keyboardDidShow(e) {
    keyboardHeight = e.endCoordinates.height;
    windowHeight = Dimensions.get('window').height;
    shortHeight = windowHeight - keyboardHeight - this.heightsSum();
    console.log("short height: ", shortHeight);
    this.setState({
      listStyle: {height: shortHeight}
    });
    this._conversationView.scrollToEnd();
  }

  keyboardDidHide() {
    this.setState({
      listStyle: {height: this.regularListHeight()}
    });
  }

  onTextInput(event) {
      var message = {
        text: event.nativeEvent.text,
        key: this.state.index.toString()
      };
      this.setState({
        input: "",
        data: [...this.state.data, message],
        index: this.state.index + 1,
        messageTypes: [...this.state.messageTypes, "outgoingMessage"],
      });
      this.fetchSomething();
      this._conversationView.scrollToEnd();
  }

  textStyle(index) {
    style = styles[this.state.messageTypes[index]];
    return style;
  }

  fetchSomething() {
    let self = this;
    fetch("https://randomuser.me/api")
      .then(
        function(response) {
          respJson = response._bodyInit;
          body = JSON.parse(respJson);
          results = body.results;
          oneResult = results[0];
          value = oneResult.name.first + " " + oneResult.name.last;
          self.setState({
            input: "",
            data: [...self.state.data, {text: value, key: self.state.index.toString()}],
            index: self.state.index + 1,
            messageTypes: [...self.state.messageTypes, "incomingMessage"],
          });
        })
      .catch(
        function (reason) {
          console.log(reason);
        });
  }

  render() {
    let pic = {
      uri: 'https://cdn.pixabay.com/photo/2019/01/16/20/52/chatbot-3936760_960_720.jpg'
    }
    return (
      <View 
        style={this.state.mainViewStyle}
        ref={(c) => this._mainView = c}
      >
        <Image source={pic} style={{width: '100%', height: heights.logo}}/>
        <Text style={styles.heading}>Welcome to BotAgg Chat!</Text>
        <Text style={styles.subtext}>Type your message below</Text>
        <View style={styles.title}><Text>Conversation</Text></View>
        <FlatList
          ref={(c) => this._conversationView = c}
          data={this.state.data}
          style={this.state.listStyle}
          renderItem={({item, index}) => <View style={styles.message}><Text style={this.textStyle(index)}>{item.text}</Text></View>}
        />
        <TextInput
          testID="messageText"
          ref={(c) => this._textInput = c}
          value={this.state.input}
          style={styles.input}
          placeholder="Your message goes here"
          onChangeText={(text) => this.setState({input: text})}
          onSubmitEditing={(event) => this.onTextInput(event)}
        ></TextInput>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 0,
    alignItems: 'stretch',
    justifyContent: 'flex-start',
    backgroundColor: '#fff'      
  },
  title: {
    height: heights.conv,
    color: 'black',
    fontSize: 14,
    justifyContent: 'flex-end'
  },
  message: {
    height: 30,
    justifyContent: 'center'
  },
  heading: {
    height: heights.head,
    color: 'black',
    fontSize: 12
  },
  subtext: {
    height: heights.subtext,
    color: 'gray',
    fontSize: 9
  },
  input: {
    height: heights.input
  },
  outgoingMessage: {
    color: 'green',
    fontSize: 14
  },
  incomingMessage: {
    color: 'red',
    fontSize: 14,
    textAlign: 'right'
  }
});
