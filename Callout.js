var React = require('react');
var AppDispatcher = require('./AppDispatcher');

module.exports = React.createClass(
    {
        forgot: function()
        {
            AppDispatcher.dispatch({operation:"got_item",
                                    data:{location:null,
                                          name:this.props.name}});
        },
        render: function()
        {
            return (<div className="horizontal_fill info_callout horizontal_layout">
                    <div className="horizontal_fill">
                    Where did you get {this.props.name}?
                    </div>
                    <button onClick={this.forgot}>I forgot</button>
                    </div>)
        }
    });



                    
