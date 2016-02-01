var React = require('react');
var AppDispatcher = require('./AppDispatcher');

module.exports = React.createClass(
    {
        login: function()
        {
            AppDispatcher.dispatch({operation:"login",
                                    data:{username:this.state.username}});
        },

        usernameChanged: function()
        {
            this.setState({username:this.refs.userName.value});
        },

        getInitialState: function()
        {
            return {username:''};
        },
        
        render: function()
        {
            return (<div className="login horizontal_fill">
                    <div>
                    <label htmlFor="username">Username:</label>
                    <input type="text" id="username" value={this.state.username} onChange={this.usernameChanged} ref="userName"/>
                    </div>
                    <button onClick={this.login}>Login</button>
                    </div>);
        }
    });
